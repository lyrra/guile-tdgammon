(use-modules (ice-9 threads))
(use-modules (statprof))

(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (ffi cblas))
(import (guile-gpu gpu))
(import (guile-gpu sigmoid))
(import (guile-machinelearning ml))
(import (guile-machinelearning mat))
(load "common-lisp.scm")
(load "common.scm")

;;; check if gpu is used


(do ((args (command-line) (cdr args)))
    ((eq? args '()))
  (cond
    ((string=? (car args) "--gpu")
     (import (guile-gpu rocm-rocblas)))))

;;; Load ML/RL

(load "backgammon.scm")
(load "td-gammon.scm")
(load "measure.scm")

(load "driver.scm")

(main)
