(define (run-tdgammon-measure-common neta netb conf)
  (set! conf (set-conf-default conf 'episodes 25))
  (set! conf (assq-set! conf 'save #f))
  (set! conf (assq-set! conf 'measure #t))
  (let* ((measure-tests (get-conf conf 'measure-tests))
         (has (lambda (x) (string-index measure-tests x)))
         (play-fun (lambda (play-type)
                     (run-episodic-selfplay tdgammon-run-episode neta play-type conf)))
         (play-compare (if (has #\c) (play-fun netb) #f))
         (play-pubeval (if (has #\p) (play-fun pubeval-best-path) #f))
         (play-random  (if (has #\r) (play-fun #:random) #f))
         (play-early   (if (has #\e) (play-fun #:early) #f))
         (play-late    (if (has #\l) (play-fun #:late) #f))
         (play-bar     (if (has #\b) (play-fun #:bar) #f))
         (play-safe    (if (has #\s) (play-fun #:safe) #f))
         (totwwin 0) (totbwin 0))
    ; sum . zip
    (if (has #\c) (set! totwwin (+ totwwin (car play-compare))))
    (if (has #\c) (set! totbwin (+ totbwin (cadr play-compare))))
    (if (has #\p) (set! totwwin (+ totwwin (car play-pubeval))))
    (if (has #\p) (set! totbwin (+ totbwin (cadr play-pubeval))))
    (if (has #\r) (set! totwwin (+ totwwin (car play-random))))
    (if (has #\r) (set! totbwin (+ totbwin (cadr play-random))))
    (if (has #\e) (set! totwwin (+ totwwin (car play-early))))
    (if (has #\e) (set! totbwin (+ totbwin (cadr play-early))))
    (if (has #\l) (set! totwwin (+ totwwin (car play-late))))
    (if (has #\l) (set! totbwin (+ totbwin (cadr play-late))))
    (if (has #\b) (set! totwwin (+ totwwin (car play-bar))))
    (if (has #\b) (set! totbwin (+ totbwin (cadr play-bar))))
    (if (has #\s) (set! totwwin (+ totwwin (car play-safe))))
    (if (has #\s) (set! totbwin (+ totbwin (cadr play-safe))))
    ; return all test results
    (list totwwin totbwin
          play-pubeval
          play-random
          play-early
          play-late
          play-bar
          play-safe
          play-compare)))

(define (run-tdgammon-measure-strength neta conf)
  (let ((nets (string-split (get-conf conf 'nets) #\,))
        (tests "prelbsc"))
    (format #t "found tests to perform: ~s~%" tests)
    (map (lambda (netb)
           (format #t "loading netb: ~s~%" netb)
           (let ((netb (file-load-net netb)))
             (match (run-tdgammon-measure-common neta netb
                                                 (assq-set! conf
                                                  'measure-tests tests))
               ((totwwin totbwin
                         pubeval
                         random
                         early
                         late
                         bar
                         safe
                         compare)
                (format #t "result: ~a/~a ~s~%"
                        totwwin totbwin
                        (list pubeval
                              random
                              early
                              late
                              bar
                              safe
                              compare))
                ))))
         nets)))

(define (run-tdgammon-measure neta netb conf)
  (cond
    ((get-conf conf 'measure-strength)
     (run-tdgammon-measure-strength neta conf))
    (else
     (match (run-tdgammon-measure-common neta netb conf)
       ((totwwin totbwin
         pubeval
         random
         early
         late
         bar
         safe
         compare)
        (let ((has (lambda (x) (string-index (get-conf conf 'measure-tests) x))))
          (display (string-concatenate (list
                    "RESULT: " (format #f "~a,~a" totwwin totbwin)
                    (if (has #\p) (format #f ",~a,~a" (car pubeval) (cadr pubeval)) "")
                    (if (has #\r) (format #f ",~a,~a" (car random)  (cadr random)) "")
                    (if (has #\e) (format #f ",~a,~a" (car early)   (cadr early)) "")
                    (if (has #\l) (format #f ",~a,~a" (car late)    (cadr late)) "")
                    (if (has #\b) (format #f ",~a,~a" (car bar)     (cadr bar)) "")
                    (if (has #\s) (format #f ",~a,~a" (car safe)    (cadr safe)) "")
                    (if (has #\c) (format #f ",~a,~a" (car compare) (cadr compare)) ""))))
          (newline)))))))
