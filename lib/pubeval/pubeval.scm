(import (system foreign))

(eval-when (compile load eval)
  (define *pubeval-dynlibfile*
    (dynamic-link (let ((lpath (getenv "GUILE_FFI_NNGPU_LIBPATH"))
                        (lname (or (getenv "GUILE_FFI_NNPUBEVAL_LIBNAME") "libnn_pubeval")))
                    (if (and lpath (not (string=? lpath "")))
                        (string-append lpath file-name-separator-string lname)
                        lname)))))

;;; pubeval API

; loads pubeval weights
(define (pubeval-rdwts)
  ((pointer->procedure
      int (dynamic-func "pubeval_rdwts" *pubeval-dynlibfile*)
      '())))

; evaluate board
(define (pubeval-eval race pos)
  ((pointer->procedure
      float (dynamic-func "pubeval_eval" *pubeval-dynlibfile*)
      (list int '*))
   race (bytevector->pointer pos)))

(define (pubeval-best-path bg paths)
  (let ((race (if (bg-race? bg) 1 0))
        (best 0) (bestval 0)
        (pos (make-s32vector 28 0)))
    (do ((i 0 (1+ i)))
        ((>= i (length paths)))
      ; setup pubeval-board
      (match (pts-ply bg)
        ((arr brr)
         (do ((p 0 (1+ p)))
             ((>= p 24))
           (let ((ap (array-ref arr p))
                 (bp (array-ref brr p)))
             (cond
              ((and (> ap 0) (> bp 0))
               ;(format #t "arr: ~s~%" arr)
               ;(format #t "brr: ~s~%" brr)
               (error "both ap and bp is set pos,ap,bp: " p ap bp))
              ((> ap 0)
               (if (pts-ply bg) ; arr=white
                 (array-set! pos ap (- 24 p))
                 (array-set! pos ap (+  1 p))))
              ((> bp 0)
               (if (pts-ply bg) ; arr=black
                 (array-set! pos (- bp) (+  1 p))
                 (array-set! pos (- bp) (- 24 p)))))))
         (cond
          ((bg-ply bg)
           (array-set! pos (bg-b-bar bg) 0)
           (array-set! pos (bg-w-bar bg) 25)
           (array-set! pos (bg-w-rem bg) 26)
           (array-set! pos (bg-b-rem bg) 27))
          (else
           (array-set! pos (bg-b-bar bg) 0)
           (array-set! pos (bg-w-bar bg) 25)
           (array-set! pos (bg-w-rem bg) 26)
           (array-set! pos (bg-b-rem bg) 27)))))
      ; evaluate board
      (let ((v (pubeval-eval race pos)))
        (if (> v bestval)
            (begin
              (set! bestval v)
              (set! best i)))))
    (list-ref paths best)))

(define (init-pubeval)
  (pubeval-rdwts))
