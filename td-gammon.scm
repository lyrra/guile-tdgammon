
(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (ffi cblas))
(load "common-lisp.scm")
(load "mat.scm")
(load "backgammon.scm")

(define (make-net)
  (let ((mhw (rand-m! (make-typed-array 'f32 *unspecified* 40 198)))
        (vho (rand-v! (make-typed-array 'f32 *unspecified* 40)))
        (myw (rand-m! (make-typed-array 'f32 *unspecified* 2 40)))
        (vyo (rand-v! (make-typed-array 'f32 *unspecified* 2))))
    (list mhw vho myw vyo)))

; Dsigmoid(x) = sigmoid(x) (1 - sigmoid(x))
(define (sigmoid vyo)
  (array-map! vyo (lambda (x)
                    (/ 1. (+ 1. (exp (- x)))))
              vyo))

; gradient-descent
; calculate gradient GRAD(weight, output)
(define (set-sigmoid-gradient! grad out)
  (array-map! grad (lambda (x)
                     (let ((s (/ 1. (+ 1. (exp (- x))))))
                       (* s (- 1. s))))
                   grad))

(define (gradient-descent vxi net err grad alpha)
  (match net
    ((mhw vho myw vyo)
     (format #t "---- gradient-descent ----~%")
     (format #t "  mhw: ~s~%" (array-dimensions mhw))
     (format #t "  vho: ~s~%" (array-dimensions vho))
     (format #t "  myw: ~s~%" (array-dimensions myw))
     (format #t "  vyo: ~s ~s~%"  (array-dimensions vyo) vyo)
     (format #t "  grad: ~s ~s~%" (array-dimensions grad) grad)
     ;  mhw: (40 198)
     ;  vho: (40)
     ;  myw: (2 40)
     ;  vyo: (2) #f32(0.495709627866745 0.46157199144363403)
     ;  grad: (2) #f32(0.25 0.25)

     ; propagate gradient backwards to output weights
     (let ((go (make-typed-array 'f32 0. 40)))
       (format #t "  propagate through output neurons~%")
       (match (array-dimensions myw)
         ((r c)
           (do ((i 0 (+ i 1))) ((= i r))
             (let ((g (array-ref grad i)))
               (do ((j 0 (+ j 1))) ((= j c))
                 (let ((w (array-ref myw i j))
                       (o (array-ref vho j)))
                   (array-set! myw (+ (array-ref myw i j) (* alpha o g)) i j)
                   ; populate gradient
                   (array-set! go (+ (array-ref go i) (* g w)) i)
                   ; each of the 40 neurons (i) in hidden layer, is connected to both neurons at output layer 
                   ; therefore foreach neuron, we sum the gradient coming from the two neurons below
                   ))))))
       (sigmoid go)
       (format #t "  propagate through hidden neurons~%")
       ; propagate gradient backwards to hidden weights
       (match (array-dimensions mhw)
         ((r c)
           (do ((i 0 (+ i 1))) ((= i r))
             (let ((g (array-ref go i)))
               (do ((j 0 (+ j 1))) ((= j c))
                 (let ((w (array-ref mhw i j))
                       (o (array-ref vxi j)))
                   (array-set! mhw (+ (array-ref mhw i j) (* alpha o g)) i j)))))))))))

(define (net-run net input)
  (match net
    ((mhw vho myw vyo)
     (sgemv! 1. mhw CblasNoTrans input 0. vho)
     (sigmoid vho)
     (sgemv! 1. myw CblasNoTrans vho 0. vyo)
     (sigmoid vyo)
     #f)))

(define *rands* #f)

(define (roll-dices)
  (let ((d1 (1+ (truncate (random 6 *rands*))))
        (d2 (1+ (truncate (random 6 *rands*)))))
    (list d1 d2)))

(define (best-path paths net)
  (let ((bout -999)
        (bpath #f)
        (vxi (make-typed-array 'f32 *unspecified* 198)))
    (loop-for path in paths do
      ;(format #t "  path: ~s~%" path)
      (let ((bg (car path)))
        (set-bg-input bg vxi #t)
        (net-run net vxi)
        (let ((out (cadddr net)))
          (if (> (array-ref out 0) bout) ; 0 is white's chance of winning
              (begin
                (set! bout (array-ref out 0))
                (set! bpath path))))))
    (if bpath ; if not terminate
        (list vxi bout bpath)
        #f)))

(define (policy-take-action bg net dices)
  (let ((paths (bg-find-all-states bg dices (bg-ply bg))))
    (best-path paths net)))

(define (sv-! dst src1 src2)
  (array-map! dst (lambda (a b)
                    (- a b))
              src1 src2))

(define (svvs*! dst vec sc)
  (array-map! dst (lambda (v) (* v sc))
              vec))

(define (run-tdgammon wnet bnet)
  ; initialize theta, given by parameters wnet and bnet
  (let ((gamma 0.9) ; td-gamma
        (gamma-lambda 0.9) ; eligibility-trace decay
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        #|
        (welig (list (make-typed-array 'f32 *unspecified* 40 198)
                     (make-typed-array 'f32 *unspecified* 2 40)))
        (belig (list (make-typed-array 'f32 *unspecified* 40 198)
                     (make-typed-array 'f32 *unspecified* 2 40)))
        |#
        (welig (make-typed-array 'f32 *unspecified* 2))
        (belig (make-typed-array 'f32 *unspecified* 2))
        (terminal-state #f))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((eq? episode 1))
      (let ((wvyo (make-typed-array 'f32 0. 2))
            (bvyo (make-typed-array 'f32 0. 2))
            (wgrad (make-typed-array 'f32 0. 2))
            (bgrad (make-typed-array 'f32 0. 2))
            (tderr (make-typed-array 'f32 0. 2)))
      ; initialize eligibily traces to 0
      (array-map! welig (lambda (x) 0.) welig)
      (array-map! belig (lambda (x) 0.) belig)
      ; set s to initial state of episode
      (set! bg (setup-bg))
      (set-bg-ply! bg #t) ; whites turn
      (set! dices (roll-dices))
      ; get initial action here
      ; Repeat for each step in episode:
      (do ((step 0 (1+ step)))
          (terminal-state)
        (let ()
          (format #t "  ~a.~a: dices: ~s white-turn: ~a~%" episode step dices (bg-ply bg))
          (bg-print-board bg)
          ; a <- pi(s)  ; set a to action given by policy for s
          ; Take action a, observe r and next state s'
          ;     new state, s', consists of bg2 and new dice-roll
          ;     s =  { bg, dices }
          ;     s' = { best-bg, new-dice-roll }
          (match (policy-take-action bg wnet dices)
            (#f ; player can't move (example is all pieces are on the bar)
             ; since we have no moves to consider/evaluate, we just yield to the other player
             (format #t "  player (~a) cant move!~%" (bg-ply bg)))
            ((vxi best-out best-path)
             (let ((bg2 (car best-path))  ; FIX: as of now, we've got the new-state in path
                   (reward 0.)
                   (wout (cadddr wnet)))
               (format #t "  best-out: ~s~%" best-out)
               (format #t "  best-path: ~s~%" best-path)
               ; we can make a move, see if the move has put us in an terminal position
               (cond
                 ; Until s' is terminal (bg2 is part of s')
                 ((or (= (bg-w-rem bg2) 15) ; white has won
                      (= (bg-b-rem bg2) 15)) ; black has won
                  (let ((ply (bg-ply bg2))) ; who's turn it was, and receives the reward
                    (set reward 1.) ; in terminal state, we get a reward of 1
                    (set! terminal-state #t))))

               ;---------------------------------------------
               ; tderr <- r + gamma * V(s') - V(s)
               ;   V(s) is previous output (wvyo/bvyo), and V(s') is wnet's output-layer
               ; gamma * V(s') - V(s)
               (format #t "  calculate tderr~%")
               (svvs*! tderr wvyo gamma)
               (sv-! tderr tderr wout)
               ; add reward
               (format #t "  add reward~%")
               (array-map! tderr (lambda (x) (+ x reward)) tderr)

               (format #t "  get gradient~%")
               ; calculate gradient GRAD(weight, output)
               (set-sigmoid-gradient! wgrad wout)

               (format #t "  update eligibily traces~%")
               ;---------------------------------------------
               ; update eligibility traces
               ; elig  <- gamma_lambda * e + Grad_theta(V(s))
               (array-map! welig (lambda (e g)
                                   (+ (* gamma-lambda e) g))
                           welig wgrad)
               (format #t "  do gradient-descent~%")
               ;---------------------------------------------
               ; update network weights
               ; theta <- theta + alpha * tderr * elig
               (gradient-descent vxi
                                 wnet (array-map! tderr (lambda (a b) (* a b))
                                                  tderr welig)
                                 wgrad ; since we already has the gradient calculated
                                       ; FIX: do we do this trick in CL version?
                                 0.2)
               (format #t "  DONE gradient-descent~%")
               ;---------------------------------------------
               ; evolve state
               ; s <- s'
               (set! bg bg2)
               (set! dices (roll-dices))
               ; caches
               (array-map! wvyo (lambda (x) x) wout)
               )))))))))

(define (main)
  (init-rand)
  (set! *rands* (seed->random-state (current-time)))
  (let* ((wnet (make-net))
         (bnet (make-net)))
    (format #t "---------------------------------~%")
    (run-tdgammon wnet bnet)
    ))

(main)
