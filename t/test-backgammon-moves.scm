(define (odd-dices)
  (let ((dices #f)
        (ok #f))
    (do ()
        (ok)
      (set! dices (roll-dices))
      (if (not (= (car dices) (cadr dices)))
          (set! ok #t)))
    dices))


(define (test-backgammon-path-1mv)
  (let ((bg (setup-bg))
        (test-depth 100))
    (array-map! (bg-b-pts bg) (lambda (x) 0) (bg-b-pts bg))
    (do ((i 0 (1+ i)))
        ((> i test-depth))
      (array-map! (bg-w-pts bg) (lambda (x) 0) (bg-w-pts bg))
      (array-set! (bg-w-pts bg) 1 (+ 6 (random 8)))
      (let ((paths (bg-find-all-states bg (odd-dices) (bg-ply bg))))
        (test-assert (= (length paths) 2) "expected 2 feasible paths")))))
