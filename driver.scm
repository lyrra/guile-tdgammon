
(define (file-load-latest-net dir)
  (let ((ds (opendir dir))
        (name #f)
        (episode #f))
    (when (directory-stream? ds)
      (do ((ent (readdir ds) (readdir ds)))
          ((eof-object? ent))
        (if (string-contains ent "net-")
          (let* ((as (substring ent (+ 4 (string-contains ent "net-"))))
                 (e (string->number (substring as 0 (string-contains as ".net")))))
            (when (or (not episode)
                      (> e episode))
              (set! episode e)
              (set! name ent)))))
      (closedir ds))
    (list name episode)))

(define (net-input-output threadio net wwin bwin episodes totsteps start-time)
  ; send current network to master
  (array-set! threadio
              (list (net-copy net)
                    wwin bwin
                    episodes
                    totsteps
                    start-time)
              1)
  ; get latest network from master
  (let ((msg (array-ref threadio 0)))
    (if (list? msg)
        (match msg
          ((new-net)
           (list new-net)))
        #f)))

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
                (file-write-net (format #f "~a-net-~a.net" fileprefix
                                        (+ start-episode totepisodes))
                                (+ start-episode totepisodes) net))))
        (sleep 4))))
  (format #t "All threads are done~%"))

(define (main)
  (sigmoid-init)
  (gpu-init)
  (let* ((net #f)
         (nets #f)
         (opponent #:self) ; default to self-play
         (measure #f)
         (threads #f)
         (threadio #f)
         (conf (make-conf
                `(verbose #f
                  profiling #f
                  threads 1
                  measure-tests "prelbs"
                  ; RL parameters
                  learn #t
                  rl-gam 0.9 ; td-gamma
                  rl-lam 0.7 ; eligibility-trace decay
                  ; ML parameters
                  waccu #f ; accumulate weights changes towards end-of-episode
                  alpha 0.1
                  numhid 40  ; number of hidden neurons
                  numout  2
                  randr #f ; randomize weights periodically
                  rande #f ; error-scaled weights randomization
                  ; environment
                  seed ,(current-time)
                  prefix "v0"
                  dir #f
                  start-episode 0))))
    (set! conf (merge-conf
                conf
                (command-line-parse
                  '((prefix string)
                    (dir string)
                    (net string)
                    (nets string)
                    (opponent string)
                    (measure string)
                    (measure-tests string)
                    (measure-strength boolean)
                    (episodes number)
                    (start-episode number)
                    (verbose boolean)
                    (profiling boolean)
                    (threads number)
                    (rl boolean)
                    (rl-lam number)
                    (rl-gam number)
                    (alpha number)
                    (numhid number)
                    (numout number)
                    (waccu boolean)
                    (randr number)
                    (rande boolean)
                    (seed number)))))
    (set! net (get-conf conf 'net))
    (let ((dir (get-conf conf 'dir)))
      (cond
       ((and (not net) dir)
        (match (file-load-latest-net dir)
          ((net2 start-episode2)
           (set! net (file-load-net (format #nil "~a/~a" dir net2)))
           (set! conf (assq-set! conf 'start-episode start-episode2)))
          (x
           (error "wrong answer from file-load-latest-net:" x))))
       (net
        (set! net (file-load-net net)))))
    (set! opponent (get-conf conf 'opponent opponent))
    (if (string? opponent)
      (if (string-contains opponent ".net")
          (set! opponent (file-load-net opponent))
          (set! opponent (symbol->keyword (string->symbol opponent)))))
    (set! nets (get-conf conf 'nets))
    (set! measure (get-conf conf 'measure))
    (if measure
      (set! measure (file-load-net measure)))
    (set! threads   (get-conf conf 'threads))
    ;
    (init-rand (get-conf conf 'seed))
    (if (not net) (set! net (make-net #:in 198
                                      #:out (get-conf conf 'numout)
                                      #:hid (get-conf conf 'numhid)
                                      #:wdelta (get-conf conf 'waccu))))
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
    (format #t "config: ~s~%" conf)
    (do ((i 0 (+ i 1)))
        ((>= i threads))
      (usleep 100000)
      (let ((thunk (lambda ()
                     (format #t "Starting thread ~a/~a~%" i threads)
                     (gpu-init-thread i)
                     (cond
                      (measure
                       (run-tdgammon-measure measure net
                                             (merge-conf
                                              conf
                                              (make-conf
                                               `(nets ,nets
                                                 thread ,i
                                                 threadio ,(if threadio
                                                             (array-ref threadio i)
                                                             #f))))))
                      (else
                       (run-episodic-selfplay tdgammon-run-episode
                                              (net-copy net) opponent
                                     (merge-conf
                                      conf
                                      (make-conf
                                       `(save #t
                                         thread ,i
                                         threadio ,(if threadio
                                                     (array-ref threadio i)
                                                     #f))))))))))
        (if (= threads 1)
            ; only do profiling if one threads is used
            (if (get-conf conf 'profiling)
                (statprof thunk)
                (thunk))
            (call-with-new-thread thunk))))
    (if (> threads 1)
        (begin
          (sleep 1)
          (handle-threads (get-conf conf 'prefix)
                          threadio
                          (get-conf conf 'start-episode)
                          100)))))

(define (run-episodic-selfplay game net oppo conf)
  ; initialize theta, given by parameters net
  (let* ((episodes (get-conf conf 'episodes))
         (start-episode (or (get-conf conf 'start-episode) 0))
         (save (get-conf conf 'save))
         (verbose (get-conf conf 'verbose))
         (thread (get-conf conf 'thread))
         (threadio (get-conf conf 'threadio))
         (measure (get-conf conf 'measure))
        ; eligibility-traces
        (rlw (if (and (get-conf conf 'learn) (not measure)) (new-rl conf net) #f))
        (rlb (if (and (get-conf conf 'learn) (not measure) (eq? oppo #:self)) (new-rl conf net) #f))
        (agentw (new-agent net rlw))
        (agentb (new-agent (if (eq? oppo #:self) net oppo) rlb))
        (wwin 0) (bwin 0)
        (start-time (current-time))
        (totsteps 0))

    ; needed because we do at first-step, randomization before weights-accumulate
    (when (get-conf conf 'waccu)
      (if rlw (net-wdelta-clear (rl-net rlw)))
      (if rlb (net-wdelta-clear (rl-net rlb))))

    (format #t "Tr:~s net: ~s~%" thread net)
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((and episodes (>= episode episodes)))
      ; merge white and black networks
      ; save the network now and then
      (if (and (not threadio) save (> episode 0) (= (modulo episode 100) 0))
          (file-write-net (format #f "~a-net-~a.net" thread
                                  (+ (or start-episode 0) episode))
                          (+ (or start-episode 0) episode) net))
      ; randomize network
      (let ((randr (get-conf conf 'randr))
            (alpha (get-conf conf 'alpha)))
        (when randr
          (cond
           ((get-conf conf 'rande)
            (let ((f (lambda (layer alpha w e)
                       (+ w (* alpha e randr (- (random-uniform) .5))))))
              (if rlw (net-weights-scale (rl-net rlw) f alpha))
              (if rlb (net-weights-scale (rl-net rlb) f alpha))))
           (else
            (let ((f (lambda (layer alpha w)
                       (+ w (* alpha randr (- (random-uniform) .5))))))
              (if rlw (net-weights-scale (rl-net rlw) f alpha))
              (if rlb (net-weights-scale (rl-net rlb) f alpha)))))))
      ; get initial action here
      ; Repeat for each step in episode:
      (match (tdgammon-run-episode rlw rlb agentw agentb #:log? (not threadio))
        ((winner steps)
         (if winner
             (set! wwin (1+ wwin))
             (set! bwin (1+ bwin)))
         (set! totsteps (+ totsteps steps))))
      ; end of episode
      ; if we are multithreading, report current net/stat
      (if threadio
          (match
           (net-input-output threadio net wwin bwin episode totsteps start-time)
            (#f #f) ; no network updates from master
            ((net) ; switch to updated networks
             (net-transfer net net)))))
    (if threadio ; signal thread done
        (array-set! threadio #:done 1))
    (list wwin bwin)))
