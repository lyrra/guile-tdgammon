
(define (handle-threads fileprefix threadio start-episode file-save-interval)
  (format #t "Waiting for threads to finish~%")
  (let ((done #f)
        (start-time (current-time))
        (episodes (make-array 0 (array-length threadio)))
        (totsteps (make-array 0 (array-length threadio)))
        (starttimes (make-array 0 (array-length threadio)))
        (wwins (make-array 0 (array-length threadio)))
        (bwins (make-array 0 (array-length threadio)))
        (net #f)
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
              ((ct-net wwin bwin ct-episodes ct-totsteps ct-start-time)
               (if (> ct-episodes (array-ref episodes i))
                   (begin
                     (array-set! episodes ct-episodes i)
                     (array-set! totsteps ct-totsteps i)
                     (array-set! starttimes ct-start-time i)
                     (array-set! wwins wwin i)
                     (array-set! bwins bwin i)
                     (if net
                         (net-merge! net net ct-net 1/6)
                         (set! net ct-net))))))))
        ; publish merged network to slave
        (do ((i 0 (+ i 1)))
            ((>= i (array-length threadio)))
          (if net
              (array-set! (array-ref threadio i)
                          (list net)
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
          (if (and net
                   (> totepisodes (+ cur-file-save-episode file-save-interval)))
              (begin
                (set! cur-file-save-episode (+ cur-file-save-episode file-save-interval))
                (file-write-net (format #f "~a-net-~a.txt" fileprefix
                                        (+ start-episode totepisodes))
                                (+ start-episode totepisodes) net))))
        (sleep 4))))
  (format #t "All threads are done~%"))

(define (main)
  (init-rand)
  (sigmoid-init)
  (gpu-init)
  (let* ((net #f)
         (opponent #:self) ; default to self-play
         (measure #f)
         (measure-tests "prelbs")
         (episodes #f)
         (start-episode 0)
         (verbose #f)
         ; ---- debug stuff ----
         (threads 1)
         (threadio #f)
         (profiling #f)
         ; ML stuff
         (numhid 40)  ; number of hidden neurons
         (rl-gam 0.9) ; td-gamma
         (rl-lam 0.7) ; eligibility-trace decay
         (file-prefix "v0"))
    (do ((args (command-line) (cdr args)))
        ((eq? args '()))
      (format #t "  arg: ~s~%" (car args))
      (if (string-contains (car args) "--prefix=")
          (set! file-prefix (substring (car args) 9)))
      (if (string-contains (car args) "--opponent=")
          (set! opponent
                (symbol->keyword (string->symbol (substring (car args) 11)))))
      (if (string-contains (car args) "--net=")
          (set! net (file-load-net (substring (car args) 6))))
      (if (string-contains (car args) "--measure=")
          (set! measure (substring (car args) 10)))
      (if (string-contains (car args) "--measure-tests=")
          (set! measure-tests (substring (car args) 16)))
      (if (string-contains (car args) "--episodes=")
          (set! episodes (string->number (substring (car args) 11))))
      (if (string-contains (car args) "--start-episode=")
          (set! start-episode (string->number (substring (car args) 16))))
      (if (string-contains (car args) "--verbose")
          (set! *verbose* #t))
      (if (string-contains (car args) "--profiling")
          (set! profiling #t))
      (if (string-contains (car args) "--threads=")
          (set! threads (string->number (substring (car args) 10))))
      ; RL parameters
      (if (string-contains (car args) "--rl-lam=") ; td-gamma
          (set! rl-lam (string->number (substring (car args) 9))))
      (if (string-contains (car args) "--rl-gam=") ; eligibility-trace
          (set! rl-gam (string->number (substring (car args) 9))))
      ; ML parameters
      (if (string-contains (car args) "--alpha=")
          (set! %alpha (string->number (substring (car args) 8))))
      (if (string-contains (car args) "--numhid=")
          (set! numhid (string->number (substring (car args) 9)))))
    (if (not net) (set! net (make-net numhid)))
    (format #t "Opponent: ~s~%" opponent)
    (if (eq? opponent #:pubeval)
      (begin
        (load "lib/pubeval/pubeval.scm")
        (pubeval-rdwts)))
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
      (usleep 100000)
      (let ((thunk (lambda ()
                     (format #t "Starting thread ~a/~a~%" i threads)
                     (gpu-init-thread i)
                     (cond
                      (measure
                       (run-tdgammon-measure measure
                                             (list (cons 'rl-gam rl-gam)
                                                   (cons 'rl-lam rl-lam))
                                             #:episodes episodes
                                             #:measure-tests measure-tests
                                             #:thread i
                                             #:threadio (if threadio
                                                            (array-ref threadio i)
                                                            #f)))
                      (else
                       (run-tdgammon (net-copy net) opponent
                                     (list (cons 'rl-gam rl-gam)
                                           (cons 'rl-lam rl-lam))
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
          (handle-threads file-prefix threadio start-episode 100)))))
