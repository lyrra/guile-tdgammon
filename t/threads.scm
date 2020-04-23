;;;; ensure parameters/fluid works

(use-modules (ice-9 threads))

(define foo #f)
(define err #f)

(define (my-run-thread)
  (foo 1)
  (usleep (truncate (random 1000)))
  (if (not (= (foo) 1))
      (set! err #t)))

(set! foo (make-parameter #f))
(do ((i 0 (+ i 1)))
    ((>= i 20))
  (usleep (truncate (random 1000)))
  (call-with-new-thread my-run-thread))
(sleep 5)
(if err (format #t "FAIL~%"))
