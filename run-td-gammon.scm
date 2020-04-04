(use-modules (statprof))

(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (ffi cblas))
(load "common-lisp.scm")
(load "common.scm")
(load "mat.scm")
(load "backgammon.scm")
(load "td-gammon.scm")

(define (main)
  (init-rand)
  (let* ((wnet (make-net))
         (bnet (make-net))
         (measure #f)
         (episodes #f)
         ; ---- debug stuff ----
         (profiling #f))
    (do ((args (command-line) (cdr args)))
        ((eq? args '()))
      (format #t "  arg: ~s~%" (car args))
      (if (string=? (car args) "--human")
          (set! wnet #:human))
      (if (string-contains (car args) "--random")
          (set! wnet #:random))
      (if (string-contains (car args) "--late")
          (set! wnet #:late))
      (if (string-contains (car args) "--early")
          (set! wnet #:early))
      (if (string-contains (car args) "--bar")
          (set! wnet #:bar))
      (if (string-contains (car args) "--swap")
          (let ((tmp wnet))
            (set! wnet bnet)
            (set! bnet tmp)))
      (if (string-contains (car args) "--wnet=")
          (set! wnet (file-load-net (substring (car args) 7) #t)))
      (if (string-contains (car args) "--bnet=")
          (set! bnet (file-load-net (substring (car args) 7) #f)))
      (if (string-contains (car args) "--measure=")
          (set! measure (substring (car args) 10)))
      (if (string-contains (car args) "--episodes=")
          (set! episodes (string->number (substring (car args) 11))))
      (if (string-contains (car args) "--profiling")
          (set! profiling #t)))

    (let ((thunk (lambda ()
                   (cond
                    (measure
                     (run-tdgammon-measure measure #:episodes episodes))
                    (else
                     (run-tdgammon wnet bnet #:save #t #:episodes episodes))))))
      (if profiling
          (statprof thunk)
          (thunk)))))

(main)
