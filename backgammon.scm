(import (ice-9 format))

(define (indent x)
  (do ((i 0 (1+ i)))
      ((>= i x))
    (format #t " ")))

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

(define (set-bg-input-pts arr vxi off)
  (do ((p 0 (1+ p)))
      ((>= p 24))
    (let ((pcs (array-ref arr p)))
      (array-set! vxi 0. (+ 0 (* p 4) off))
      (array-set! vxi 0. (+ 1 (* p 4) off))
      (array-set! vxi 0. (+ 2 (* p 4) off))
      (array-set! vxi 0. (+ 3 (* p 4) off))
      (if (> pcs 3) (array-set! vxi (/ (- pcs 3) 2) (+ 3 (* p 4) off)))
      (if (> pcs 2) (array-set! vxi 1. (+ 2 (* p 4) off)))
      (if (> pcs 1) (array-set! vxi 1. (+ 1 (* p 4) off)))
      (if (> pcs 0) (array-set! vxi 1. (+ 0 (* p 4) off))))))

(define (pts-ply bg)
  (let  ((arr (bg-w-pts bg))
         (brr (bg-b-pts bg)))
    (if (bg-ply bg)
      (list arr brr)
      (list brr arr))))

(define (bg-print-board bg)
  (let ((old-ply (bg-ply bg)))
    (set-bg-ply! bg #t)
  (match (pts-ply bg)
    ((arr brr)
     (do ((p 0 (1+ p)))
         ((>= p 24))
       (format #t "~2d  " p))
     (format #t "~%")
     (do ((p 0 (1+ p)))
         ((>= p 24))
       (let ((apcs (array-ref arr p)))
         (if (> apcs 0)
           (format #t "~2d  " apcs)
           (format #t "    "))))
     (format #t "[b:~a, r:~a]~%" (bg-w-bar bg) (bg-w-rem bg))
     (do ((p 0 (1+ p)))
         ((>= p 24))
       (let ((bpcs (array-ref brr p)))
         (if (> bpcs 0)
             (format #t "~2d  " bpcs)
             (format #t "    "))))
     (format #t "[b:~a, r:~a]~%" (bg-b-bar bg) (bg-b-rem bg))
     (format #t "~%")))
  (set-bg-ply! bg old-ply)))

; input features as specified by Tesauro's td-gammon
(define (set-bg-input bg vxi)
  ; 192 inputs decode for white+black * 4input * 24 points
  (let ((n 0))
    (match (pts-ply bg)
      ((arr brr)
       (set-bg-input-pts arr vxi n)
       (set! n (+ n (* 4 24)))
       (set-bg-input-pts brr vxi n)
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


(define (bg-apply-move bg oldpos newpos newpcs ply)
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
            (bg-fold-states nbg (cdr dices))))
         (else ; position is occupied
          (if (eq? '() (cdr dices))
            '() ; no more dices, don't include this state
            (bg-fold-states bg (cdr dices)))))))
     (else
      (let ((paths '()))
        (do ((p 0 (1+ p)))
            ((>= p 24))
          (let ((pcs (array-ref arr p)))
            (if (> pcs 0) ; point carries a piece
                (let* ((newpos (+ p (* (car dices) dir)))
                       (newpcs (1- pcs)))
                  ; validate move
                  (if (or (< newpos 0) ; piece has moved outside of board
                          (> newpos 23) ; piece has moved outside of board
                          ; if piece lands on board, it mustn't be occupied
                          (< (array-ref brr newpos) 2)) ; max one opponent piece
                    (let ((nbg (bg-apply-move (copy-bg bg) p newpos newpcs ply)))
                      (set! paths
                            (append paths
                                    (bg-fold-states nbg (cdr dices))))))))))
        (if (eq? paths '())
            (list bg)
            paths)))))))))

(define (bg-find-all-states bg dices)
  (let ((d1 (car dices))
        (d2 (cadr dices)))
    ;(format #t "  find-all-states ply:~a dice: [~a,~a]~%" ply d1 d2)
    ; scan all possible moves in 'arr' using dices d1 and d2
    (bg-fold-states bg
                    (cond
                     ((> d2 d1) (list d2 d1)) ; d1 must be >= d2
                     ((= d2 d1) (list d1 d1 d1 d1))
                     (else (list d1 d2))))))
