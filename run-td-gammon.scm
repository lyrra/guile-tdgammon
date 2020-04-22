(use-modules (ice-9 threads))
(use-modules (statprof))

(import (srfi srfi-1) (ice-9 match) (srfi srfi-8) (srfi srfi-9))
(import (ffi cblas))
(load "common-lisp.scm")
(load "common.scm")
(load "mat.scm")
(load "net.scm")
(load "rl.scm")
(load "backgammon.scm")
(load "td-gammon.scm")

(load "sigmoid.scm")
(sigmoid-init)

(define (handle-threads fileprefix threadio start-episode file-save-interval)
  (format #t "Waiting for threads to finish~%")
  (let ((done #f)
        (start-time (current-time))
        (episodes (make-array 0 (array-length threadio)))
        (totsteps (make-array 0 (array-length threadio)))
        (starttimes (make-array 0 (array-length threadio)))
        (wwins (make-array 0 (array-length threadio)))
        (bwins (make-array 0 (array-length threadio)))
        (wnet #f) (bnet #f)
        (cur-file-save-episode 0))
    (do ()
        (done)
      (let ((count 0))
        ; go through all compute-threads, get their networks and statistics
        (do ((i 0 (+ i 1)))
            ((>= i (array-length threadio)))
          (let ((msg (array-ref (array-ref threadio i) 1)))
            (match msg
              (0 #f) ; no news
              (#:done  ; compute-thread is done
                (set! count (1+ count)))
              ((ct-wnet ct-bnet wwin bwin ct-episodes ct-totsteps ct-start-time)
               (if (> ct-episodes (array-ref episodes i))
                   (begin
                     (array-set! episodes ct-episodes i)
                     (array-set! totsteps ct-totsteps i)
                     (array-set! starttimes ct-start-time i)
                     (array-set! wwins wwin i)
                     (array-set! bwins bwin i)
                     (if wnet
                         (net-merge! wnet wnet ct-wnet)
                         (set! wnet ct-wnet))
                     (if bnet
                         (net-merge! bnet bnet ct-bnet)
                         (set! bnet ct-bnet))))))))
        ; publish merged network
        (do ((i 0 (+ i 1)))
            ((>= i (array-length threadio)))
          (if (and wnet bnet)
              (array-set! (array-ref threadio i)
                          (list wnet bnet)
                          0)))
        ; exit if all compute-threads are done
        (if (= count (array-length threadio))
            (set! done #t))
        ; print some statistics
        (let ((sumtotsteps 0)
              (tid (current-time))
              (totepisodes 0)
              (wwin 0)
              (bwin 0))
          (array-for-each
           (lambda (episode totstep starttime ww bw)
             (set! sumtotsteps (+ sumtotsteps totstep))
             (set! totepisodes (+ totepisodes episode))
             (set! wwin        (+ wwin ww))
             (set! bwin        (+ bwin bw)))
           episodes totsteps starttimes wwins bwins)
          (format #t "~a e:~a c/t:~a W/B: ~4f cu: ~s episodes:~s~%"
                  (+ start-episode totepisodes)
                  totepisodes
                  (truncate (/ sumtotsteps
                               (- (current-time)
                                  start-time -1)))
                  (/ wwin (1+ bwin))
                  totsteps
                  episodes)
          ; save latest network
          (if (and wnet bnet
                   (> totepisodes (+ cur-file-save-episode file-save-interval)))
              (begin
                (set! cur-file-save-episode (+ cur-file-save-episode file-save-interval))
                (file-write-net (format #f "~a-net-~a.txt" fileprefix
                                        (+ start-episode totepisodes))
                                (+ start-episode totepisodes) wnet bnet))))
        (sleep 4))))
  (format #t "All threads are done~%"))

(define (main)
  (init-rand)
  (let* ((wnet (make-net))
         (bnet (make-net))
         (measure #f)
         (episodes #f)
         (start-episode #f)
         (verbose #f)
         ; ---- debug stuff ----
         (threads 1)
         (threadio #f)
         (profiling #f))
    (do ((args (command-line) (cdr args)))
        ((eq? args '()))
      (format #t "  arg: ~s~%" (car args))
      (if (string=? (car args) "--human")
          (set! wnet #:human))
      (if (string-contains (car args) "--random")
          (set! wnet #:random))
      (if (string-contains (car args) "--late")
          (set! wnet #:late))
      (if (string-contains (car args) "--early")
          (set! wnet #:early))
      (if (string-contains (car args) "--bar")
          (set! wnet #:bar))
      (if (string-contains (car args) "--safe")
          (set! wnet #:safe))
      (if (string-contains (car args) "--swap")
          (let ((tmp wnet))
            (set! wnet bnet)
            (set! bnet tmp)))
      (if (string-contains (car args) "--wnet=")
          (set! wnet (file-load-net (substring (car args) 7) #t)))
      (if (string-contains (car args) "--bnet=")
          (set! bnet (file-load-net (substring (car args) 7) #f)))
      (if (string-contains (car args) "--measure=")
          (set! measure (substring (car args) 10)))
      (if (string-contains (car args) "--episodes=")
          (set! episodes (string->number (substring (car args) 11))))
      (if (string-contains (car args) "--start-episode=")
          (set! start-episode (string->number (substring (car args) 16))))
      (if (string-contains (car args) "--verbose")
          (set! *verbose* #t))
      (if (string-contains (car args) "--profiling")
          (set! profiling #t))
      (if (string-contains (car args) "--threads=")
          (set! threads (string->number (substring (car args) 10)))))
    (if (> threads 1)
        (begin
          (set! threadio (make-array #f threads))
          (array-map! threadio (lambda (x)
                                 (let ((slot (make-array #f 2)))
                                   (array-map! slot (lambda (x) 0) slot)
                                   slot))
                      threadio)))
    (do ((i 0 (+ i 1)))
        ((>= i threads))
      (let ((thunk (lambda ()
                     (format #t "Starting thread ~a/~a~%" i threads)
                     (cond
                      (measure
                       (run-tdgammon-measure measure #:episodes episodes
                                             #:thread i
                                             #:threadio (if threadio
                                                            (array-ref threadio i)
                                                            #f)))
                      (else
                       (run-tdgammon wnet bnet
                                     #:save #t #:episodes episodes
                                     #:start-episode start-episode
                                     #:verbose verbose #:thread i
                                     #:threadio (if threadio
                                                    (array-ref threadio i)
                                                    #f)))))))
        (if (= threads 1)
            ; only do profiling if one threads is used
            (if profiling
                (statprof thunk)
                (thunk))
            (call-with-new-thread thunk))))
    (if (> threads 1)
        (begin
          (sleep 1)
          (handle-threads "1" threadio start-episode 100)))))

(main)
