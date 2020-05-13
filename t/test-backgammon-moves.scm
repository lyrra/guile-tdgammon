
(define (odd-dices)
  (let ((dices #f)
        (ok #f))
    (do ()
        (ok)
      (set! dices (roll-dices))
      (if (not (= (car dices) (cadr dices)))
          (set! ok #t)))
    dices))

(define (dices->pos dices)
  (match dices
    ((a b)
     (if (= a b) (* a 4) (+ a b)))))

(define-test (test-backgammon-path-edge)
  (let ((bg (setup-bg)))
    (array-zero! (bg-b-pts bg))
    (array-zero! (bg-w-pts bg))
    (array-inc! (bg-w-pts bg)  0 1)
    (array-inc! (bg-b-pts bg) 23 1)
    (let ((paths (bg-find-all-states bg (odd-dices))))
      (test-assert (= (length paths) 1)
                   (format #f "expected 1 feasible path, got ~a"
                           (length paths))))))

(define-test (test-backgammon-path-1mv)
  (let ((bg (setup-bg)))
    (array-zero! (bg-b-pts bg))
    (loop-subtests (i)
      (array-zero! (bg-w-pts bg))
      (array-inc! (bg-w-pts bg) (+ 6 (random 8)) 1)
      (let ((paths (bg-find-all-states bg (odd-dices))))
        (test-assert (= (length paths) 1)
                     (format #f "expected 1 feasible path, got ~a"
                             (length paths)))))))

(define-test (test-backgammon-path-2mv)
  (let ((bg (setup-bg)))
    (array-zero! (bg-b-pts bg))
    (loop-subtests (i)
      (array-zero! (bg-w-pts bg))
      (array-inc! (bg-w-pts bg) (+ 6 (random 8)) 1)
      (array-inc! (bg-w-pts bg) (+ 6 (random 8)) 1)
      (let ((paths (bg-find-all-states bg (odd-dices))))
        (test-assert (> (length paths) 1)
                     (format #f "expected >1 feasible paths, got ~a"
                             (length paths)))))))

(define (_test-backgammon-bar-pos_ ply)
  (let* ((bg (setup-bg))
         (dices (odd-dices))
         (pos (dices->pos dices)))
    (set-bg-ply! bg ply)
    (array-zero! (bg-b-pts bg))
    (array-zero! (bg-w-pts bg))
    (cond
     (ply
      (set-bg-w-bar! bg 1)
      (set-bg-w-rem! bg 14))
     (else
      (set-bg-b-bar! bg 1)
      (set-bg-b-rem! bg 14)))
    (let ((paths (bg-find-all-states bg dices)))
        (test-assert (= (length paths) 1)
                     (format #f "expected 1 feasible path, got ~a"
                             (length paths)))
        (let ((bg2 (car paths)))
          (cond
           (ply
            (array-for-each (lambda (x) (assert (= x 0)))
                            (bg-b-pts bg2))
            (loop-array (lambda (i x)
                          (if (= i (- 24 pos))
                             (test-assert (= x 1) "white-wrong-1-pos")
                             (test-assert (= x 0) "white-wrong-0-pos")))
                        (bg-w-pts bg2)))
           (else
            (array-for-each (lambda (x) (assert (= x 0)))
                            (bg-w-pts bg2))
            (loop-array (lambda (i x)
                          (if (= i (1- pos))
                             (test-assert (= x 1) "black-wrong-1-pos")
                             (test-assert (= x 0) "black-wrong-0-pos")))
                        (bg-b-pts bg2))))))))


(define-test (test-backgammon-bar-pos)
  (let ((bg (setup-bg)))
    (loop-subtests (i)
      (_test-backgammon-bar-pos_ #t)
      (_test-backgammon-bar-pos_ #f))))

(define (_test-backgammon-state-valid bg)
  (match (pts-ply bg)
    ((arr brr)
     (do ((p 0 (1+ p)))
         ((>= p 24))
       ; same point musn't be occupied by both players
       (assert (not (and (> (array-ref arr p) 0)
                         (> (array-ref brr p) 0))))))))

(define (_test-backgammon-run-game pre-step-fun move-fun)
  (let* ((bg (setup-bg))
         (dices (odd-dices))
         (pos (dices->pos dices))
         (terminal-state #f)
         (ply #t)) ; white begin
      (do ((step 0 (1+ step)))
          (terminal-state)
        (pre-step-fun bg step)
        (match (move-fun bg (roll-dices))
          (#f ; player cant move
           (test-assert (not (state-terminal? bg))))
          (new-bg
           (set! terminal-state (state-terminal? new-bg))
           (set! bg new-bg)
           (set-bg-ply! bg ply))))))

(define-test (test-backgammon-valid-pos)
  (let ((pre-step-fun (lambda (bg step)
                        (_test-backgammon-state-valid bg)))
        (move-fun (lambda (bg dices)
                    (let ((paths (bg-find-all-states bg dices)))
                      (loop-for bg in paths do
                        (_test-backgammon-state-valid bg))
                      (list-ref paths (random (length paths)))))))
    (loop-subtests (i)
      (_test-backgammon-run-game pre-step-fun move-fun))))
