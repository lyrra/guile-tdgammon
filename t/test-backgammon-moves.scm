(define (odd-dices)
  (let ((dices #f)
        (ok #f))
    (do ()
        (ok)
      (set! dices (roll-dices))
      (if (not (= (car dices) (cadr dices)))
          (set! ok #t)))
    dices))

(define (array-inc! arr pos val)
  (let ((v (array-ref arr pos)))
    (array-set! arr (+ v val) pos)))

(define (test-backgammon-path-edge)
  (let ((bg (setup-bg)))
    (array-map! (bg-b-pts bg) (lambda (x) 0) (bg-b-pts bg))
    (array-map! (bg-w-pts bg) (lambda (x) 0) (bg-w-pts bg))
    (array-inc! (bg-w-pts bg)  0 1)
    (array-inc! (bg-b-pts bg) 23 1)
    (let ((paths (bg-find-all-states bg (odd-dices))))
      (test-assert (= (length paths) 1)
                   (format #f "expected 1 feasible path, got ~a"
                           (length paths))))))

(define (test-backgammon-path-1mv)
  (let ((bg (setup-bg))
        (test-depth 100))
    (array-map! (bg-b-pts bg) (lambda (x) 0) (bg-b-pts bg))
    (do ((i 0 (1+ i)))
        ((> i test-depth))
      (array-map! (bg-w-pts bg) (lambda (x) 0) (bg-w-pts bg))
      (array-inc! (bg-w-pts bg) (+ 6 (random 8)) 1)
      (let ((paths (bg-find-all-states bg (odd-dices))))
        (test-assert (= (length paths) 1)
                     (format #f "expected 1 feasible path, got ~a"
                             (length paths)))))))

(define (test-backgammon-path-2mv)
  (let ((bg (setup-bg))
        (test-depth 100))
    (array-map! (bg-b-pts bg) (lambda (x) 0) (bg-b-pts bg))
    (do ((i 0 (1+ i)))
        ((> i test-depth))
      (array-map! (bg-w-pts bg) (lambda (x) 0) (bg-w-pts bg))
      (array-inc! (bg-w-pts bg) (+ 6 (random 8)) 1)
      (array-inc! (bg-w-pts bg) (+ 6 (random 8)) 1)
      (let ((paths (bg-find-all-states bg (odd-dices))))
        (test-assert (> (length paths) 1)
                     (format #f "expected >1 feasible paths, got ~a"
                             (length paths)))))))
