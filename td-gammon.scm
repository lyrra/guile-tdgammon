
(define (roll-dices)
  (let ((d1 (1+ (truncate (random-number 6))))
        (d2 (1+ (truncate (random-number 6)))))
    (list d1 d2)))

(define (policy-take-action bg net dices action-topn)
  (let ((paths (let ((lst (bg-find-all-states bg dices)))
                 (if (bg-ply bg)
                     lst (reverse lst)))))
    (if action-topn
        (rl-policy-greedy-action-topn net bg paths set-bg-input 
                                      (1+ (random-number (1- action-topn))))
        (rl-policy-greedy-action net bg paths set-bg-input))))

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


(define (run-turn bg agent dices action-topn)
  (let ((net (agent-net agent)))
    (cond
     ((netr? net) ; player is controlled by artificial type of neural-network
      (policy-take-action bg net dices action-topn))
     ((eq? net #:human) ; human type of neural-network controls player
      (human-take-action bg dices))
     (else
      (style-take-action bg dices net)))))

(define* (tdgammon-run-episode rlw rlb agentw agentb #:key log? action-topn)
  (let ((bg (setup-bg)) ; set s to initial state of episode
        (terminal-state #f)
        (winner #f)
        (dices (roll-dices))
        (steps 0))
    (set-bg-ply! bg #t) ; whites turn
    (do ((step 0 (1+ step)))
        (terminal-state)
      (let ((ply (bg-ply bg))
            (sel-action-topn #f))
        (LLL "~a: ~a/~a dices: ~s bar:[~a,~a] rem:[~a,~a]~%"
             step
             (if (bg-ply bg) "WHITE" "BLACK")
             (if (eq? (agent-net (if ply agentw agentb)) #:human) "HUMAN" "COMPU")
             dices
             (bg-w-bar bg) (bg-b-bar bg)
             (bg-w-rem bg) (bg-b-rem bg))
        (if *verbose* (bg-print-board bg))
        (if (and rlw (= step 0)) (agent-init agentw bg set-bg-input))
        (if (and rlb (= step 1)) (agent-init agentb bg set-bg-input))
        (if action-topn
            (match action-topn
              ((all white black)
               (if (bg-ply bg)
                 (set! sel-action-topn (or all white))
                 (set! sel-action-topn (or all black))))))
        ; a <- pi(s)  ; set a to action given by policy for s
        ; Take action a, observe r and next state s'
        ;     new state, s', consists of bg2 and new dice-roll
        ;     s =  { bg, dices }
        ;     s' = { best-bg, new-dice-roll }
        (match (run-turn bg (if ply agentw agentb) dices sel-action-topn)
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
             (array-scopy! (agent-ovxi (if ply agentb agentw)) (net-vxi (agent-net agentw)))
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
          (set! winner #t))
         ((= (bg-b-rem bg) 15)
          (assert terminal-state)
          (set! winner #f)))
        (when terminal-state
          (set! steps step)
          (if (or log? *verbose*)
            (format #t "winner:~a~%" (if (= (bg-w-rem bg) 15) "WHITE" "BLACK"))))
        ;--------------------------------------
        ; s <- s' , dices are part of state/env
        (set! dices (roll-dices))
        (set-bg-ply! bg (not ply))
        ; bookkeeping
        (bg-validate bg)))
    (list winner steps)))
