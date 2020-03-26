
(define (test-backgammon-path-1mv)
  (let ((bg (setup-bg)))
    ; setup board
    (array-map! (bg-w-pts bg) (lambda (x) 0) (bg-w-pts bg))
    (array-set! (bg-w-pts bg) 1 5) 
    (let ((paths (bg-find-all-states bg (roll-dices) (bg-ply bg))))
      (test-assert (= (length paths) 2) "expected 2 feasible paths"))))
