
(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (ffi cblas))
(load "mat.scm")
(load "backgammon.scm")

(define (make-net)
  (let ((mhw (rand-m! (make-typed-array 'f32 *unspecified* 40 198)))
        (vho (rand-v! (make-typed-array 'f32 *unspecified* 40)))
        (myw (rand-m! (make-typed-array 'f32 *unspecified* 2 40)))
        (vyo (rand-v! (make-typed-array 'f32 *unspecified* 2))))
    (list mhw vho myw vyo)))

(define (net-run net input)
  (match net
    ((mhw vho myw vyo)
     (format #t "vho: ~s~%" vho)
     (sgemv! 1. mhw CblasNoTrans input 0. vho)
     (sgemv! 1. myw CblasNoTrans vho 0. vyo)
     #f)))

(define (main)
  (init-rand)
  (let* ((randstate (seed->random-state (current-time)))
         (bgb (setup-bg))
         (net (make-net))
         (vxi (make-typed-array 'f32 *unspecified* 198)))
    (format #t "---------------------------------~%")
    (format #t " inputs: ~a~%" (set-bg-input bgb vxi #t))

    (let* ((d1 (1+ (truncate (random 6))))
           (d2 (1+ (truncate (random 6))))
           (bg-states (bg-find-all-states bgb d1 d2 #t)))
      (format #t "bg-states: ~s~%"
              (map (lambda (x)
                     (if (bg? x)
                         "bg"
                         x))
                   bg-states))
      )

    (net-run net vxi)
    (format #t " output: ~a~%" (cadddr net))
    ))

(main)
