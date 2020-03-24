
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

    (let* ((d1 (1+ (truncate (random 6 randstate))))
           (d2 (1+ (truncate (random 6 randstate))))
           (x (bg-print-board bgb))
           (bg-paths (bg-find-all-states bgb
                                         6 ;(if (> d1 d2) d1 d2)
                                         4 ;(if (> d2 d1) d1 d2)
                                         #t)))
      ;-------------
      ; take the state-tree and compile a feasible-state-list
      (format #t "number of paths: ~s~%" (length bg-paths))
      ; evaluate each path
      (loop-for path in bg-paths do
        (format #t "  path: ~s~%" path)
        (let ((bg (car path)))
          (format #t " inputs: ~a~%" (set-bg-input bg vxi #t))
          (net-run net vxi)
          (format #t " output: ~a~%" (cadddr net))))
      ;-------------
      #f)))

(main)
