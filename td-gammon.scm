
(define (roll-dices)
  (let ((d1 (1+ (truncate (random-number 6))))
        (d2 (1+ (truncate (random-number 6)))))
    (list d1 d2)))

(define (policy-take-action bg net dices)
  (let ((paths (let ((lst (bg-find-all-states bg dices)))
                 (if (bg-ply bg)
                     lst (reverse lst)))))
    (rl-policy-greedy-action net bg paths)))

(define (human-select-action bg dices paths)
  (let ((action 0)
        (filter #f))
    (while #t
      (let* ((i 0) ; print moves, maybe using a filter
             (str (format #f "~a" filter))
             (ptsf (if (string=? "m" (substring str 0 1))
                       (1- (string->number (substring str 1)))
                       #f)))
        (loop-for path in paths do
          (let* ((safe (bg-safe? path #f))
                 (bar  (> (bg-w-bar path) 0))
                 (rem  (> (bg-b-rem path) (bg-b-rem bg)))
                 (prip (lambda ()
                         (format #t "~a ~a~a~a: [w b:~s r:~s] [b b:~s r:~s] {" i
                                 (if safe "s" " ")
                                 (if bar  "b" " ")
                                 (if rem  "r" " ")
                                 (bg-w-bar path)
                                 (bg-w-rem path)
                                 (bg-b-bar path)
                                 (bg-b-rem path))
                         (do ((p 0 (1+ p)))
                             ((>= p 24))
                           (let ((pts (array-ref (bg-w-pts path) p)))
                             (if (= 0 pts)
                               (format #t ".")
                               (format #t "~a" pts))))
                         (format #t "} {")
                         (do ((p 0 (1+ p)))
                             ((>= p 24))
                           (let ((pts (array-ref (bg-b-pts path) p)))
                             (if (= 0 pts)
                               (format #t ".")
                               (format #t "~a" pts))))
                         (format #t "} ~a~%" i))))
          (cond
           ((and ptsf (> (array-ref (bg-b-pts path) ptsf) 0)) (prip))
           ((and (eq? 'a filter) (or bar safe)) (prip))
           ((and (eq? 'b filter) bar) (prip))
           ((and (eq? 's filter) safe) (prip))
           ((not filter) (prip)))
          (set! i (1+ i)))))
      (let ((i 0)  ; count types of moves
            (b 0) ; put white points on bar
            (s 0) ; safe move for black
            (c 0)) ; combo
        (loop-for path in paths do
          (let ((bar (> (bg-w-bar path) 0))
                (safe (bg-safe? path #f)))
            (if (and bar safe) (set! c (1+ c)))
            (if bar (set! b (1+ b)))
            (if safe (set! s (1+ s)))))
        (format #t "~%bar: ~a, safe: ~a combo: ~a~%" b s c))
      (bg-print-board bg)
      (format #t "(~s) dices: ~a Please select path> " filter dices)
      (set! filter #f)
      (let ((n (read)))
        (cond
         ((number? n)
          (set! action n)
          (break))
         ((eq? 'q n) (set! filter #f))
         (else
          (set! filter n)))))
    (list-ref paths action)))

(define (human-take-action bg dices)
  (let ((paths (bg-find-all-states bg dices)))
    (cond
      ((eq? paths '()) #f) ; no paths to take
      (else
       (human-select-action bg dices paths)))))

(define (style-take-action bg dices style)
  (let ((paths (bg-find-all-states bg dices))
        (i 0))
    (if (= (length paths) 0)
        #f
        (cond
      ((eq? style #:random)
       (list-ref paths (random-number (length paths))))
      ((eq? style #:early)
       (car paths))
      ((eq? style #:late)
       (car (last-pair paths)))
      ((eq? style #:bar)
       (let ((sel #f))
         (loop-for path in paths do
           (if (> (if (bg-ply bg)
                      (bg-b-bar path)
                      (bg-w-bar path))
                  0)
               (set! sel path)))
         (or sel (car (last-pair paths)))))
      ((eq? style #:safe)
       (let ((sels (filter (lambda (path)
                             (let ((arr (if (bg-ply bg)
                                            (bg-w-pts path)
                                            (bg-b-pts path)))
                                   (safe #t))
                               (array-for-each (lambda (x)
                                                 (if (= x 1) ; exposed position
                                                     (set! safe #f)))
                                               arr)
                               safe))
                           paths)))
         (if (eq? sels '())
           (car (last-pair paths))
           (car (last-pair sels)))))
      ((procedure? style) (style bg paths))))))


(define (run-turn bg agent dices)
  (let ((net (agent-net agent)))
    (cond
     ((array? net) ; player is controlled by artificial type of neural-network
      (policy-take-action bg net dices))
     ((eq? net #:human) ; human type of neural-network controls player
      (human-take-action bg dices))
     (else
      (style-take-action bg dices net)))))

(define* (run-tdgammon net oppo opts #:key episodes start-episode save verbose thread threadio
                       measure)
  ; initialize theta, given by parameters net
  (format #t "Tr:~s net: ~s~%" thread net)
  (let* ((bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (rlw (if (and (get-opt opts 'learn) (not measure)) (new-rl opts net) #f))
        (rlb (if (and (get-opt opts 'learn) (not measure) (eq? oppo #:self)) (new-rl opts net) #f))
        (agentw (new-agent net rlw))
        (agentb (new-agent (if (eq? oppo #:self) net oppo) rlb))
        (wwin 0) (bwin 0)
        (terminal-state #f)
        (start-time (current-time))
        (totsteps 0))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((and episodes (>= episode episodes)))
      ; merge white and black networks
      ; save the network now and then
      (if (and (not threadio) save (> episode 0) (= (modulo episode 100) 0))
          (file-write-net (format #f "~a-net-~a.net" thread
                                  (+ (or start-episode 0) episode))
                          (+ (or start-episode 0) episode) net))
      ; set s to initial state of episode
      (set! bg (setup-bg))
      (set-bg-ply! bg #t) ; whites turn
      (set! dices (roll-dices))
      (set! terminal-state #f)
      ; get initial action here
      ; Repeat for each step in episode:
      (do ((step 0 (1+ step)))
          (terminal-state)
        (let ((ply (bg-ply bg)))
          (LLL "~a.~a.~a: [~a/~a] dices: ~s w/b-turn: ~a bar:[~a,~a] rem:[~a,~a]~%"
                  thread episode step wwin bwin dices (bg-ply bg)
                  (bg-w-bar bg) (bg-b-bar bg)
                  (bg-w-rem bg) (bg-b-rem bg))
          (if *verbose* (bg-print-board bg))
          (if (and rlw (= step 0)) (agent-init agentw bg))
          (if (and rlb (= step 1)) (agent-init agentb bg))
          ; a <- pi(s)  ; set a to action given by policy for s
          ; Take action a, observe r and next state s'
          ;     new state, s', consists of bg2 and new dice-roll
          ;     s =  { bg, dices }
          ;     s' = { best-bg, new-dice-roll }
          (match (run-turn bg (if ply agentw agentb) dices)
            (#f ; player can't move (example is all pieces are on the bar)
              ; since we have no moves to consider/evaluate, we just yield to the other player
             (assert (not (state-terminal? bg)))
             'ok)
            (new-bg
             (set! terminal-state (state-terminal? new-bg))
             (if (if ply rlw rlb) ; let ML-player learn the step
               (run-ml-learn new-bg
                             (if ply rlw rlb)
                             (get-reward new-bg ply)))
             ; if in terminal-state, also learn the loser experience
             (when (and terminal-state (if ply rlb rlw)) ; ML-player
               (assert (state-terminal? new-bg) "loser in non-terminal")
               ; since we are using the same network (self-play) bring back the old/half-step input
               (array-scopy! (agent-ovxi (if ply agentb agentw)) (net-vxi net))
               (run-ml-learn new-bg
                             (if ply rlb rlw) ; use the previous turns player
                             (get-reward new-bg (not ply))))
             ; evolve state
             (agent-end-turn (if ply agentw agentb))
             ; s <- s'
             (set! bg new-bg)))
          ; check if terminal-state
          (cond
           ((= (bg-w-rem bg) 15)
            (assert terminal-state)
            (set! wwin (+ wwin 1)))
           ((= (bg-b-rem bg) 15)
            (assert terminal-state)
            (set! bwin (+ bwin 1))))
          (if terminal-state
            (begin
              (set! totsteps (+ totsteps step))
            (if (not threadio)
              (format #t "~a.~a.~a s/t:~a winner:~a [~a,~a]~%" thread episode step
                      ;(inexact->exact (truncate (/ totsteps (- (current-time) start-time -1))))
                      (truncate (/ totsteps (- (current-time) start-time -1)))
                      (if (= (bg-w-rem bg) 15) "WHITE" "BLACK")
                      wwin bwin))))
          ;--------------------------------------
          ; s <- s' , dices are part of state/env
          (set! dices (roll-dices))
          (set-bg-ply! bg (not ply))
          ; bookkeeping
          (bg-validate bg)))
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
