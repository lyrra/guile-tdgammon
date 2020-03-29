
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
     ;  mhw: (40 198)
     ;  vho: (40)
     ;  myw: (2 40)
     ;  vyo: (2) #f32(0.495709627866745 0.46157199144363403)
     ;  grad: (2) #f32(0.25 0.25)

     ; propagate gradient backwards to output weights
     (let ((go (make-typed-array 'f32 0. 40)))
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

(define (best-path paths net out-idx)
  (let ((bout -999)
        (bpath #f)
        (bvxi (make-typed-array 'f32 *unspecified* 198))
        (vxi (make-typed-array 'f32 *unspecified* 198)))
    (loop-for path in paths do
      ;(format #t "  path: ~s~%" path)
      (let ((bg path))
        (set-bg-input bg vxi #t)
        (net-run net vxi)
        (let ((out (cadddr net)))
          ; FIX: should we consider white(idx-0) > black(idx-1) ?
          (if (> (array-ref out out-idx) bout) ; 0 is white's chance of winning, 1 is black
              (begin
                ;(format #t "  best-net-out: ~s~%" out)
                (set! bout (array-ref out out-idx))
                (set! bpath path)
                (array-map! bvxi (lambda (x) x) vxi))))))
    (if bpath ; if not terminate
        (list bvxi bout bpath)
        #f)))

(define (policy-take-action bg net dices)
  (let ((paths (bg-find-all-states bg dices)))
    (best-path paths net (if (bg-ply bg) 0 1))))

(define (sv-! dst src1 src2)
  (array-map! dst (lambda (a b)
                    (- a b))
              src1 src2))

(define (svvs*! dst vec sc)
  (array-map! dst (lambda (v) (* v sc))
              vec))

(define (get-reward bg)
  ; we can make a move, see if the move has put us in an terminal position
  (cond
    ; Until s' is terminal (bg2 is part of s')
    ((or (= (bg-w-rem bg) 15) ; white has won
         (= (bg-b-rem bg) 15)) ; black has won
     (let ((ply (bg-ply bg))) ; who's turn it was, and receives the reward
       ; in terminal state, we get a reward of 1
       (assert (= (if (bg-ply bg) (bg-w-rem bg) (bg-b-rem bg)) 15))
       (list 1. #t)))
    (else
     (list 0. #f))))

(define (run-tderr vxi net tderr vyo nout grad elig reward gamma gamma-lambda)
  (let ()
    ;---------------------------------------------
    ; tderr <- r + gamma * V(s') - V(s)
    ;   V(s) is previous output (wvyo/bvyo), and V(s') is wnet's output-layer
    ; gamma * V(s') - V(s)
    (svvs*! tderr vyo gamma)
    (sv-! tderr tderr nout)
    ; add reward
    (array-map! tderr (lambda (x r) (+ x r)) tderr reward)

    ; calculate gradient GRAD(weight, output)
    (set-sigmoid-gradient! grad nout)

    ;---------------------------------------------
    ; update eligibility traces
    ; elig  <- gamma_lambda * e + Grad_theta(V(s))
    (array-map! elig (lambda (e g)
                       (+ (* gamma-lambda e) g))
                elig grad)
    ;---------------------------------------------
    ; update network weights
    ; theta <- theta + alpha * tderr * elig
    (gradient-descent vxi
                      net (array-map! tderr (lambda (a b) (* a b))
                                      tderr elig)
                      grad ; since we already has the gradient calculated
                            ; FIX: do we do this trick in CL version?
                      ; alpha
                      0.3)))

(define (run-turn bg net dices tderr vyo grad elig gamma gamma-lambda)
  (let ((ply (bg-ply bg)))
    ;(format #t "  run-turn ply=~a, dices=~s~%" ply dices)
    (match (policy-take-action bg net dices)
      (#f ; player can't move (example is all pieces are on the bar)
       ; since we have no moves to consider/evaluate, we just yield to the other player
       ;(format #t "  player (~a) cant move!~%" (bg-ply bg))
       #f)
      ((vxi best-out best-path)
       (let ((bg2 best-path)
             (nout (cadddr net)))
         (match (get-reward bg2)
           ((reward terminal-state)
            (let ((rewarr (make-typed-array 'f32 0. 2)))
              (if (> reward 0)
                (begin
                  (array-set! rewarr 1. (if ply 0 1))
                  (array-set! rewarr -1. (if ply 1 0))))
              (run-tderr vxi net tderr vyo nout grad elig rewarr gamma gamma-lambda))
            ; caches
            (array-map! vyo (lambda (x) x) nout)
            (list bg2 terminal-state))))))))

(define (run-tdgammon wnet bnet)
  ; initialize theta, given by parameters wnet and bnet
  (let ((gamma 0.9) ; td-gamma
        (gamma-lambda 0.9) ; eligibility-trace decay
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (welig (make-typed-array 'f32 *unspecified* 2))
        (belig (make-typed-array 'f32 *unspecified* 2))
        (terminal-state #f))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((= episode 10))
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
      (set! terminal-state #f)
      ; get initial action here
      ; Repeat for each step in episode:
      (do ((step 0 (1+ step)))
          (terminal-state)
        (let ((ply (bg-ply bg)))
          (format #t "~a.~a: dices: ~s w/b-turn: ~a bar:[~a,~a] rem:[~a,~a]~%"
                  episode step dices (bg-ply bg)
                  (bg-w-bar bg) (bg-b-bar bg)
                  (bg-w-rem bg) (bg-b-rem bg))
          (bg-print-board bg)
          ; a <- pi(s)  ; set a to action given by policy for s
          ; Take action a, observe r and next state s'
          ;     new state, s', consists of bg2 and new dice-roll
          ;     s =  { bg, dices }
          ;     s' = { best-bg, new-dice-roll }
          (match (run-turn bg (if ply wnet bnet)
                           dices tderr
                           (if ply wvyo bvyo)
                           (if ply wgrad bgrad)
                           (if ply welig belig)
                           gamma gamma-lambda)
            (#f 'ok) ; cant move
            ((new-bg is-terminal-state)
             ; evolve state
             ; s <- s'
             (set! bg new-bg)
             (set! terminal-state is-terminal-state)))
          (set! dices (roll-dices)) ; also part of state
          (set-bg-ply! bg (not ply))
          ; bookkeeping
          ; ensure always 15 pieces is present on board+removed+bar
          (let ((wtot 0)
                (btot 0))
            (array-for-each (lambda (x) (set! wtot (+ wtot x))) (bg-w-pts bg))
            (array-for-each (lambda (x) (set! btot (+ btot x))) (bg-b-pts bg))
            (set! wtot (+ wtot (bg-w-rem bg)))
            (set! wtot (+ wtot (bg-w-bar bg)))
            (set! btot (+ btot (bg-b-rem bg)))
            (set! btot (+ btot (bg-b-bar bg)))
            (assert (= wtot 15) (format #f "w-pcs/=15:~a" wtot))
            (assert (= btot 15) (format #f "b-pcs/=15:~a" btot))
            )))))))
