(import (ice-9 format))

(define-record-type <bg>
  (make-bg)
  bg?
  (ply bg-ply set-bg-ply!) ; #t = white's turn
  (w-bar bg-w-bar set-bg-w-bar!)
  (b-bar bg-b-bar set-bg-b-bar!)
  (w-rem bg-w-rem set-bg-w-rem!)
  (b-rem bg-b-rem set-bg-b-rem!)
  (w-pts bg-w-pts set-bg-w-pts!)
  (b-pts bg-b-pts set-bg-b-pts!))

(define (setup-bg)
  (let ((bg (make-bg)))
    (set-bg-ply! bg #t)
    (set-bg-w-bar! bg 0)
    (set-bg-b-bar! bg 0)
    (set-bg-w-rem! bg 0)
    (set-bg-b-rem! bg 0)
    (let ((warr (make-array 0 24))
          (barr (make-array 0 24)))
      ; white/black initial pieces position at the points
      (array-set! warr 2 23) (array-set! barr 2 0)
      (array-set! warr 5 12) (array-set! barr 5 11)
      (array-set! warr 3  7) (array-set! barr 3 16)
      (array-set! warr 5  5) (array-set! barr 5 18)
      (set-bg-w-pts! bg warr)
      (set-bg-b-pts! bg barr))
    bg))

(define (copy-bg src)
  (let ((dst (make-bg)))
    (set-bg-ply!   dst (bg-ply src))
    (set-bg-w-bar! dst (bg-w-bar src))
    (set-bg-b-bar! dst (bg-b-bar src))
    (set-bg-w-rem! dst (bg-w-rem src))
    (set-bg-b-rem! dst (bg-b-rem src))
    (set-bg-w-pts! dst (make-array 0 24))
    (set-bg-b-pts! dst (make-array 0 24))
    (let ((wsrc (bg-w-pts src))
          (bsrc (bg-b-pts src))
          (wdst (bg-w-pts dst))
          (bdst (bg-b-pts dst)))
      (do ((i 0 (1+ i)))
          ((>= i 24))
        (array-set! wdst (array-ref wsrc i) i)
        (array-set! bdst (array-ref bsrc i) i)))
    dst))


(define (pts-ply bg)
  (let  ((arr (bg-w-pts bg))
         (brr (bg-b-pts bg)))
    (if (bg-ply bg)
      (list arr brr)
      (list brr arr))))

(define (bg-print-board bg)
  (let ()
    (format #t "  ")
    (do ((p 0 (1+ p)))
        ((>= p 24))
      (format #t "~a" (if (= 0 (modulo p 6)) "|" " "))
      (format #t "~2d " (1+ p)))
    (format #t "~%W: ")
    (do ((p 0 (1+ p)))
        ((>= p 24))
      (let ((pcs (array-ref (bg-w-pts bg) p))
            (m (format #f "~a" (if (= 0 (modulo (1+ p) 6)) "|" " "))))
        (cond
         ((> pcs 9) (format #t "+ ~a " m))
         ((= pcs 0) (format #t "   ~a" m))
         (else (format #t "~2d ~a" pcs m)))))
    (format #t "  bar: ~d rem: ~d~%" (bg-w-bar bg) (bg-w-rem bg))
    ; print black
    (format #t "B: ")
    (do ((p 0 (1+ p)))
        ((>= p 24))
      (let ((pcs (array-ref (bg-b-pts bg) p))
            (m (format #f "~a" (if (= 0 (modulo (1+ p) 6)) "|" " "))))
        (cond
         ((> pcs 9) (format #t "+ ~a  " m))
         ((= pcs 0) (format #t "   ~a" m))
         (else (format #t "~2d ~a" pcs m)))))
    (format #t "  bar: ~d rem: ~d~%" (bg-b-bar bg) (bg-b-rem bg))))

(define (bg-safe? path side)
  (let ((safe #t))
    (do ((i 0 (1+ i)))
        ((or (not safe) (>= i 24)))
      (if (= (array-ref (if side (bg-w-pts path) (bg-b-pts path))
                        i)
             1)
          (set! safe #f)))
    safe))

; input features as specified by Tesauro's td-gammon
(define (set-bg-input bg vxi)
  (define (set-bg-input-pts-w arr vxi off)
    (do ((p 0 (1+ p)))
        ((>= p 24))
      (let ((pcs (array-ref arr p)))
        (array-set! vxi (if (> pcs 0) 1.              0.) (+ 0 off))
        (array-set! vxi (if (> pcs 1) 1.              0.) (+ 1 off))
        (array-set! vxi (if (> pcs 2) 1.              0.) (+ 2 off))
        (array-set! vxi (if (> pcs 3) (/ (- pcs 3) 2) 0.) (+ 3 off))
        (set! off (+ off 4)))))
  (define (set-bg-input-pts-b arr vxi off)
    (do ((p 23 (1- p)))
        ((< p 0))
      (let ((pcs (array-ref arr p)))
        (array-set! vxi (if (> pcs 0) 1.              0.) (+ 0 off))
        (array-set! vxi (if (> pcs 1) 1.              0.) (+ 1 off))
        (array-set! vxi (if (> pcs 2) 1.              0.) (+ 2 off))
        (array-set! vxi (if (> pcs 3) (/ (- pcs 3) 2) 0.) (+ 3 off))
        (set! off (+ off 4)))))
  ; 192 inputs decode for white+black * 4input * 24 points
  (let ((n 0))
    (match (pts-ply bg)
      ((arr brr)
       (if (bg-ply bg)
         (set-bg-input-pts-w arr vxi n)
         (set-bg-input-pts-b arr vxi n))
       (set! n (+ n (* 4 24)))
       (if (bg-ply bg)
         (set-bg-input-pts-w brr vxi n)
         (set-bg-input-pts-b brr vxi n))
       (set! n (+ n (* 4 24)))
       (cond
        ((bg-ply bg)
         (array-set! vxi (/ (bg-w-bar bg)  2) n)
         (array-set! vxi (/ (bg-b-bar bg)  2) (+ 1 n))
         (array-set! vxi (/ (bg-w-rem bg) 15) (+ 2 n))
         (array-set! vxi (/ (bg-b-rem bg) 15) (+ 3 n))
         (array-set! vxi 1. (+ 4 n)) ; not used, but serves as a bias-neuron
         (array-set! vxi 0. (+ 5 n)))
        (else
         (array-set! vxi (/ (bg-b-bar bg)  2) n)
         (array-set! vxi (/ (bg-w-bar bg)  2) (+ 1 n))
         (array-set! vxi (/ (bg-b-rem bg) 15) (+ 2 n))
         (array-set! vxi (/ (bg-w-rem bg) 15) (+ 3 n))
         (array-set! vxi 1. (+ 4 n)) ; not used, but serves as a bias-neuron
         (array-set! vxi 0. (+ 5 n))))
       (set! n (+ n 6))
       n))))

(define (bg-race? bg)
  (and (= (bg-w-bar bg) 0)
       (= (bg-b-bar bg) 0)
       (let ((w 23) (b 0)
             (wrr (bg-w-pts bg))
             (brr (bg-b-pts bg)))
         (do ((p 0 (1+ p)))
             ((>= p 24))
           (if (> (array-ref brr p) 0) (set! b p)))
         (do ((p 23 (1- p)))
             ((>= 0 p))
           (if (> (array-ref wrr p) 0) (set! w p)))
         ; if black and white has passed each other, it is a race!
         ; else they are still in contact
         (> b w))))


(define (bg-apply-move bg oldpos newpos ply)
  (match (pts-ply bg)
    ((arr brr)
     ; remove A-piece from old-position
     (array-set! arr (1- (array-ref arr oldpos)) oldpos)
     (cond
      ((or (< newpos 0)
           (> newpos 23))
       ;(format #t "move outside ~a~%" (if ply "w" "b"))
       ; A-piece moved out of board
       (if ply
           (set-bg-w-rem! bg (1+ (bg-w-rem bg)))
           (set-bg-b-rem! bg (1+ (bg-b-rem bg)))))
      (else ; A-piece has landed on new position on board
       ;(format #t "move inside board ~a ~a->~a~%" (if ply "w" "b") oldpos newpos)
       (array-set! arr (1+ (array-ref arr newpos)) newpos)
       ; knock out B-piece
       (if (= (array-ref brr newpos) 1)
           (begin
             ;(format #t "knock ~a at pos:~a" (if ply "b" "w") newpos)
             (array-set! brr 0 newpos)
             (if ply
                 (set-bg-b-bar! bg (1+ (bg-b-bar bg)))
                 (set-bg-w-bar! bg (1+ (bg-w-bar bg))))))))))
  ; why return anything here?
  ; perhaps return old board if move is not feasible? ie, opponent block
  bg)

; FIX: storing path instead of state would be less memory consuming
; returns a list of paths
(define (bg-fold-states bg dices)
  (let ((ply (bg-ply bg)))
  (match (pts-ply bg)
    ((arr brr)
  ; scan all possible moves in 'arr' using dices d1 and d2
  (let ((dir (if ply -1 1))) ; white moves towards 0, black towards 24
    (cond
     ; no more dices to evaluate moves
     ((= (length dices) 0)
      (list bg)) ; we return a list of paths
     ; ply has pieces on bar, must move them first
     ((> (if ply (bg-w-bar bg) (bg-b-bar bg)) 0)
      (let* ((d (car dices))
             ; white/black start of opposite side
             (newpos (if ply (- 24 d) (- d 1))))
        (cond
         ; position is possible to move in to
         ((< (array-ref brr newpos) 2)
          (let ((nbg (copy-bg bg)))
            ; put piece on the board
            (array-inc! (if ply (bg-w-pts nbg) (bg-b-pts nbg))
                        newpos 1)
            (cond ; remove piece from bar
             (ply
              (set-bg-w-bar! nbg (1- (bg-w-bar nbg))))
             (else
              (set-bg-b-bar! nbg (1- (bg-b-bar nbg)))))
            ; if opponent is hit, move to bar
            (if (= (array-ref brr newpos) 1)
                (begin
                (array-set! (if ply (bg-b-pts nbg) (bg-w-pts nbg))
                            0
                            newpos)
                (if ply
                    (set-bg-b-bar! nbg (1+ (bg-b-bar nbg)))
                    (set-bg-w-bar! nbg (1+ (bg-w-bar nbg))))))
            (bg-fold-states nbg (cdr dices))))
         (else ; position is occupied
          '()))))
     (else
      (let ((paths '()))
        (do ((p 0 (1+ p)))
            ((>= p 24))
          (let ((pcs (array-ref arr p)))
            (if (> pcs 0) ; point carries a piece
                (let ((newpos (+ p (* (car dices) dir))))
                  ; validate move
                  (if (or (and (< newpos 0)  ; whites piece has moved outside of board
                               (let ((v #t)) ; ensure no points are outside home
                                 (do ((i 6 (1+ i)))
                                     ((>= i 24))
                                   (if (> (array-ref arr i) 0)
                                       (set! v #f)))
                                 v)
                               ; ensure move is exact-outside, OR, no higher pieces exist
                               (or (= newpos -1) ; exactly outside
                                   (let ((good #t))
                                     (do ((i (1+ p) (1+ i)))
                                         ((>= i 24))
                                       (if (> (array-ref arr i) 0)
                                           (set! good #f)))
                                     good)))
                          (and (> newpos 23) ; blacks piece has moved outside of board
                               (let ((v #t)) ; ensure no points are outside home
                                 (do ((i 0 (1+ i)))
                                     ((>= i 18))
                                   (if (> (array-ref arr i) 0)
                                       (set! v #f)))
                                 v)
                               ; ensure move is exact-outside, OR, no higher pieces exist
                               (or (= newpos 24) ; exactly outside
                                   (let ((good #t))
                                     (do ((i 0 (1+ i)))
                                         ((>= i p))
                                       (if (> (array-ref arr i) 0)
                                           (set! good #f)))
                                     good)))
                          ; if piece lands on board, it mustn't be occupied
                          (and (>= newpos 0) (< newpos 24)
                               (< (array-ref brr newpos) 2))) ; max one opponent piece
                    (let ((nbg (bg-apply-move (copy-bg bg) p newpos ply)))
                      (set! paths
                            (append paths
                                    (bg-fold-states nbg (cdr dices))))))))))
        (if (eq? paths '())
            (list bg)
            paths)))))))))

(define (bg-array-equal? pts1 pts2)
  (let ((q #t))
    (do ((i 0 (1+ i)))
        ((>= i (array-length pts1)))
      (if (not (= (array-ref pts1 i)
                  (array-ref pts2 i)))
          (set! q #f)))
    q))

(define (bg-state-equal? bg1 bg2)
  (and (eq? (bg-ply bg1) (bg-ply bg2))
       (= (bg-w-bar bg1) (bg-w-bar bg2))
       (= (bg-b-bar bg1) (bg-b-bar bg2))
       (= (bg-w-rem bg1) (bg-w-rem bg2))
       (= (bg-b-rem bg1) (bg-b-rem bg2))
       (bg-array-equal? (bg-w-pts bg1) (bg-w-pts bg2))
       (bg-array-equal? (bg-b-pts bg1) (bg-b-pts bg2))))

(define (bg-find-all-states bg dices)
  (let ((d1 (car dices))
        (d2 (cadr dices)))
    ;(format #t "  find-all-states ply:~a dice: [~a,~a]~%" ply d1 d2)
    ; scan all possible moves in 'arr' using dices d1 and d2
    (let ((paths (bg-fold-states bg (cond
                                     ((> d2 d1) (list d2 d1)) ; d1 must be >= d2
                                     ((= d2 d1) (list d1 d1 d1 d1))
                                     (else (list d1 d2)))))
          (paths2 (if (not (= d1 d2))
                      (bg-fold-states bg (cond
                                          ((> d2 d1) (list d1 d2))
                                          (else (list d2 d1))))
                      #f))
          (paths3 '()))
      (do ((pp paths (cdr pp)))
          ((eq? '() pp))
        (let ((p (car pp))
              (found #f))
          (do ((pp3 paths3 (cdr pp3)))
              ((eq? '() pp3))
            (let ((p3 (car pp3)))
              (if (bg-state-equal? p p3)
                  (set! found #t))))
          (if (not found)
              (set! paths3 (cons p paths3)))))

      (when paths2
        (do ((pp2 paths2 (cdr pp2)))
            ((eq? '() pp2))
          (let ((p2 (car pp2))
                (found #f))
            (do ((pp3 paths3 (cdr pp3)))
                ((eq? '() pp3))
              (let ((p3 (car pp3)))
                (if (bg-state-equal? p2 p3)
                    (set! found #t))))
            (if (not found)
                (set! paths3 (cons p2 paths3))))))
      paths3)))

(define (state-terminal? bg)
  (or (= (bg-w-rem bg) 15) ; white has won
      (= (bg-b-rem bg) 15))) ; black has won


(define (bg-validate bg)
  ; ensure always 15 pieces is present on board+removed+bar
  (let ((wtot 0)
        (btot 0))
    (array-for-each (lambda (x) (set! wtot (+ wtot x))) (bg-w-pts bg))
    (array-for-each (lambda (x) (set! btot (+ btot x))) (bg-b-pts bg))
    (set! wtot (+ wtot (bg-w-rem bg)))
    (set! wtot (+ wtot (bg-w-bar bg)))
    (set! btot (+ btot (bg-b-rem bg)))
    (set! btot (+ btot (bg-b-bar bg)))
    (assert (= wtot 15) (format #f "w-pcs/=15:~a" wtot))
    (assert (= btot 15) (format #f "b-pcs/=15:~a" btot))))

(define (get-reward bg ply)
  ; assuming we can make a move, see if the move has put us in an terminal position
  (cond
    ; Until s' is terminal (bg2 is part of s')
    ((state-terminal? bg)
     (let ((next-ply (bg-ply bg))) ; who's turn it was, and receives the reward
       ; in terminal state, we get a reward of 1
       (assert (= (if next-ply (bg-w-rem bg) (bg-b-rem bg)) 15) "get-reward, not terminal!")
       (if (eq? ply next-ply)
         (list 1. #t)
         (list -1. #t))))
    (else
     (list 0. #f))))
