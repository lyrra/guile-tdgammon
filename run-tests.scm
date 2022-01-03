;
;
;
(use-modules (ice-9 threads))
(use-modules (statprof))
(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (guile-gpu gpu))
(import (guile-gpu sigmoid))
(import (guile-gpu mat))

(import (guile-ml ml))

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


(load "t/test-common.scm") ; test driver
;;; tests
(load "t/test-backgammon-moves.scm")
(load "t/test-backgammon-td.scm")

(begin
  (gpu-init)
  (sigmoid-init)
  (init-rand)
  (join-thread
   (call-with-new-thread 
    (lambda ()
      (gpu-init-thread 0)
      (run-tests '(test-backgammon-valid-pos
                   test-backgammon-bar-pos
                   test-backgammon-path-edge
                   test-backgammon-path-1mv test-backgammon-path-2mv
                   test-backgammon-path-dual
                   test-backgammon-remove-pts
                   test-backgammon-bar
                   test-backgammon-td-run-out1 ; 1-neuron output
                   test-backgammon-td-run-out2 ; 2-neuron output
                   ))))))
