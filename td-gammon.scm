
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
  (let ((best-out -999)
        (best-path #f)
        (vxi (make-typed-array 'f32 *unspecified* 198)))
    (loop-for path in bg-paths do
      ;(format #t "  path: ~s~%" path)
      (let ((bg (car path)))
        (set-bg-input bg vxi #t)
        (net-run net vxi)
        (let ((out (cadddr net)))
          (if (> (array-ref out 0) best-out) ; 0 is white's chance of winning
              (begin
                (set! best-out (array-ref out 0))
                (set! best-path path))))))
    (if best-path ; if not terminate
        (list best-out best-path)
        #f)))

(define (policy-take-action bg net dices)
  (let ((bg-paths (bg-find-all-states bg dices (bg-ply bg))))
    (best-path bg-paths net)))

(define (run-tdgammon wnet bnet)
  ; initialize theta, given by parameters wnet and bnet
  (let ((gamma 0.9)
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (welig (list (make-typed-array 'f32 *unspecified* 40 198)
                     (make-typed-array 'f32 *unspecified* 2 40)))
        (belig (list (make-typed-array 'f32 *unspecified* 40 198)
                     (make-typed-array 'f32 *unspecified* 2 40)))
        (terminal-state #f))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((eq? episode 1))
      ; initialize eligibily traces to 0
      (set! welig (map (lambda (arr) (array-map! arr (lambda (x) 0.) arr)) welig))
      (set! belig (map (lambda (arr) (array-map! arr (lambda (x) 0.) arr)) belig))
      ; set s to initial state of episode
      (set! bg (setup-bg))
      (set! dices (roll-dices))
      ; Repeat for each step in episode:
      (do ((step 0 (1+ step)))
          (terminal-state)
        (set-bg-ply! bg #t) ; whites turn
        (format #t "  ~a.~a: dices: ~s white-turn: ~a~%" episode step dices (bg-ply bg))
        (bg-print-board bg)
        ; a <- pi(s)  ; set a to action given by policy for s
        (let ((reward 0.))
          (match (policy-take-action bg wnet dices)
            (#f ; terminate
             ; get reward (who won)

             ; Take action a, observe r and next state s'

             (set! reward 1.)
             (set! terminal-state #t))
            ((best-out best-path)
             (format #t "  best-out: ~s~%" best-out)
             (format #t "  best-path: ~s~%" best-path)
             ; Take action a, observe r and next state s'
             ; (perform-path bg best-path) we've got the new-state in path
             (set! bg (car best-path))
             ; tderr <- r + gamma * V(s') - V(s)

             ; elig  <- gamma_lambda * e + Grad_theta(V(s))
             ; theta <- theta + alpha * tderr * elig
             ; s <- s'
             ; Until s' is terminal
             )))
        (set! terminal-state #t)
          ))))

(define (main)
  (init-rand)
  (set! *rands* (seed->random-state (current-time)))
  (let* ((wnet (make-net))
         (bnet (make-net)))
    (format #t "---------------------------------~%")
    (run-tdgammon wnet bnet)
    (exit)

      ;-------------
      ; take the state-tree and compile a feasible-state-list
      ; evaluate each path
      (let ((best-out -999)
            (best-path #f))
        (loop-for path in bg-paths do
          ;(format #t "  path: ~s~%" path)
          (let ((bg (car path)))
            (set-bg-input bg vxi #t)
            (net-run net vxi)
            (let ((out (cadddr net)))
              (if (> (array-ref out 0) best-out) ; 0 is white's chance of winning
                (begin
                  (set! best-out (array-ref out 0))
                  (set! best-path path))))))
        (format #t "  best-out: ~s~%" best-out)
        (format #t "  best-path: ~s~%" best-path))
      ;-------------
      #f)))

(main)
