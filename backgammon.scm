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
      (cond
       ((> pcs 3) (array-set! vxi (/ (- pcs 3) 2) (+ 3 (* p 4) off)))
       ((= pcs 3) (array-set! vxi 1. (+ 2 (* p 4) off)))
       ((= pcs 2) (array-set! vxi 1. (+ 1 (* p 4) off)))
       ((= pcs 1) (array-set! vxi 1. (+ 0 (* p 4) off)))))))

(define (pts-ply bg ply)
  (let  ((arr (bg-w-pts bg))
         (brr (bg-b-pts bg)))
    (if (not ply) ; black's move, swap
        (let ((tmp arr))
          (set! arr brr)
          (set! brr tmp)))
    (list arr brr)))

(define (bg-print-board bg)
  (match (pts-ply bg #t)
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
     (format #t "~%")
     (do ((p 0 (1+ p)))
         ((>= p 24))
       (let ((bpcs (array-ref brr p)))
         (if (> bpcs 0)
             (format #t "~2d  " bpcs)
             (format #t "    "))))
     (format #t "~%"))))

; input features as specified by Tesauro's td-gammon
(define (set-bg-input bg vxi ply)
  ; 192 inputs decode for white+black * 4input * 24 points
  (let ((n 0))
    (match (pts-ply bg ply)
      ((arr brr)
       (set-bg-input-pts arr vxi n)
       (set! n (+ n (* 4 24)))
       (set-bg-input-pts brr vxi n)
       (set! n (+ n (* 4 24)))
       (cond
        (ply
         (array-set! vxi (/ (bg-w-bar bg)  2) n)
         (array-set! vxi (/ (bg-b-bar bg)  2) (+ 1 n))
         (array-set! vxi (/ (bg-w-rem bg) 15) (+ 2 n))
         (array-set! vxi (/ (bg-b-rem bg) 15) (+ 3 n))
         (array-set! vxi 1. (+ 4 n))
         (array-set! vxi 0. (+ 5 n)))
        (else
         (array-set! vxi (/ (bg-b-bar bg)  2) n)
         (array-set! vxi (/ (bg-w-bar bg)  2) (+ 1 n))
         (array-set! vxi (/ (bg-b-rem bg) 15) (+ 2 n))
         (array-set! vxi (/ (bg-w-rem bg) 15) (+ 3 n))
         (array-set! vxi 1. (+ 4 n))
         (array-set! vxi 0. (+ 5 n))))
       (set! n (+ n 6))
       n))))

(define (bg-apply-move bg oldpos newpos newpcs ply)
  (match (pts-ply bg ply)
    ((arr brr)
     (cond
      (ply
       ; remove A-piece from old-position
       (array-set! arr (1- (array-ref arr oldpos)) oldpos)
       (cond
        ((or (< newpos 0)
             (> newpos 23))
         ; A-piece moved out of board
         (set-bg-w-rem! bg (1+ (bg-w-rem bg))))
        (else ; A-piece has landed on new position on board
         (array-set! arr (1+ (array-ref arr newpos)) newpos)
         ; knock out B-piece
         (if (= (array-ref brr newpos) 1)
             (begin
               (array-set! brr 0 newpos)
               (set-bg-b-bar! bg (1+ (bg-b-bar bg)))
               )))))
      (else
       ; remove B-piece from old-position
       (array-set! brr (1- (array-ref brr oldpos)) oldpos)
       (cond
        ((or (< newpos 0)
             (> newpos 23))
         ; B-piece moved out of board
         (set-bg-b-rem! bg (1+ (bg-b-rem bg))))
        (else ; B-piece has landed on new position on board
         (array-set! brr (1+ (array-ref brr newpos)) newpos)
         ; knock out A-piece
         (if (= (array-ref arr newpos) 1)
             (begin
               (array-set! arr 0 newpos)
               (set-bg-w-bar! bg (1+ (bg-w-bar bg)))))))))))
  bg)


; returns a list of paths
(define (bg-fold-states path bg ply dices)
  ;(let ((rr (random 9999)))
  ;(format #t "    ~a-- bg-fold-states -- paths:~s dices=~s ~%" rr (length path) dices)
  (match (pts-ply bg ply)
    ((arr brr)
  ;(indent (* 2 in))
  ;(format #t "  bg-fold-states dices=~s~%" dices)
  ; scan all possible moves in 'arr' using dices d1 and d2
  (let ((dir (if ply -1 1))) ; white moves towards 0, black towards 24
    (cond
     ; no more dices to evaluate moves
     ((= (length dices) 0)
       ;(indent (* 2 (length path)))
       ;(format #t "    no more dice ~s~%" dices)
      ;(format #t "       ~a-- no-dices! --~%" rr)
      (list (append (list bg) path))) ; we return a list of paths
     ; ply has pieces on bar, must move them first
     ((> (if ply (bg-w-bar bg) (bg-b-bar bg)) 0)
      (indent (* 2 (length path)))
      ;(format #t "    ~a-- bar-move~s~%" rr)
      (let ((d (car dices)))
        (cond
         ; position is possible to move in to
         ((< (array-ref bar (if ply (- 24 d) (- 1 d))) 2)
          (let ((nbg (copy-bg bg)))
            (cond
             (ply
              (set-bg-w-bar! nbg (1- (bg-w-bar! nbg))) ; remove piece from bar
              (array-set! (bg-w-pts nbg) ; put piece on the board
                          (1+ (bg-w-pts nbg))))
             (else
              (set-bg-b-bar! nbg (1- (bg-b-bar! nbg)))
              (array-set! (bg-b-pts nbg)
                          (1+ (bg-b-pts nbg)))))
            (let ((act (list 'bar d)))
              (bg-fold-states (append path (list act)) nbg ply (cdr dices)))))
         (else ; position is occupied
          (bg-fold-states path bg ply (cdr dices))))))
     (else
      ;(format #t "      ~a-- else-move --~%" rr)
      (let ((paths '()))
        (do ((p 0 (1+ p)))
            ((>= p 24))
          (let ((pcs (array-ref arr p)))
            (if (> pcs 0) ; point carries a piece
                (let* ((newpos (+ p (* (car dices) dir)))
                       (newpcs (1- pcs)))
                  ;(format #t "        ~a-- do: ~a, pcs: ~a dices:~s (mov-pos ~a to ~a)~%" rr p pcs dices p newpos)
                  ;(indent (* 2 (length path))
                  ;(format #t "    consider p=~a pcs=~a dices=~s newpos=~a~%" p pcs dices newpos)
                  ; validate move
                  (if (or (< newpos 0) ; piece has moved outside of board
                          (> newpos 23) ; piece has moved outside of board
                          ; if piece lands on board, it mustn't be occupied
                          (< (array-ref brr newpos) 2)) ; max one opponent piece
                    (let ((nbg (bg-apply-move (copy-bg bg) p newpos newpcs ply)))
                      (let ((act (list 'mov p newpos newpcs)))
                        (indent (* 2 (length path)))
                        ;(format #t "    ~a-- nop=~a feasible move act=~s dices=~s~%" rr (length path) act dices)
                        (set! paths
                              (append paths
                                      (bg-fold-states (append path (list act)) nbg ply (cdr dices))))
                        ;(format #t "    ~a-- new paths is: ~s~%" rr paths)
                        )))))))
        ;(format #t "  ~a-- paths: ~s~%" rr paths)
        ;(if (> (length paths) 0) (format #t "    ~a-- found paths: ~s~%" rr paths))
        (append path paths)))))))
  ;)
  )

(define (bg-find-all-states bg dices ply)
  (let ((d1 (car dices))
        (d2 (cadr dices)))
    (format #t "find-all-states white: ~a dice: [~a,~a]~%" ply d1 d2)
    ; scan all possible moves in 'arr' using dices d1 and d2
    (bg-fold-states '() bg ply
                    (cond
                     ((> d2 d1) (list d2 d1)) ; d1 must be >= d2
                     ((= d2 d1) (list d1 d1 d1 d1))
                     (else (list d1 d2))))))
