(use-modules (ice-9 threads))
(use-modules (statprof))

(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (ffi cblas))
(import (guile-gpu common))
(import (guile-gpu gpu))
(import (guile-gpu sigmoid))
(load "common-lisp.scm")
(load "common.scm")
(load "bio.scm")
(load "mat.scm")
;(load "sigmoid.scm")
;(load "gpu.scm")

;;; check if gpu is used


(do ((args (command-line) (cdr args)))
    ((eq? args '()))
  (cond
    ((string=? (car args) "--gpu")
     (import (guile-gpu rocm-rocblas)))))

;;; Load ML/RL

(load "net.scm")
(load "rl.scm")
(load "agent.scm")
(load "backgammon.scm")
(load "td-gammon.scm")
(load "measure.scm")

(load "driver.scm")

(main)
