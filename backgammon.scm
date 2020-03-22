
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
      ; initial pieces position at the points
      (array-set! warr 2 0) (array-set! barr 2 0)
      (array-set! warr 5 11) (array-set! barr 5 11)
      (array-set! warr 5 11) (array-set! barr 5 11)
      (array-set! warr 3 16) (array-set! barr 3 16)
      (array-set! warr 5 18) (array-set! barr 5 18)
      (set-bg-w-pts! bg warr)
      (set-bg-b-pts! bg barr))
    bg))

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

; input features as specified by Tesauro's td-gammon
(define (set-bg-input bg vxi ply)
  ; 192 inputs decode for white+black * 4input * 24 points
  (let ((n 0)
        (arr (bg-w-pts bg))
        (brr (bg-b-pts bg)))
    (if (not ply) ; black's move, swap
        (let ((tmp arr))
          (set! arr brr)
          (set! brr tmp)))
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
    n))
