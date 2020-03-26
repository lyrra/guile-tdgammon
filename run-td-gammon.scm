
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
         (bnet (make-net)))
    (run-tdgammon wnet bnet)))

(main)
