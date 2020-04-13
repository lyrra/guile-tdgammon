
(define (file-load-net file which)
  (let ((net #f))
    (call-with-input-file file
      (lambda (p)
        (let ((x (read p)))
          (match (car x)
            (#:episode ;old network type
             (set! net
                   (car (cdr (if which (caddr x) (cadddr x)))))
             ; if net is saved as a list, convert to vector
             (if (list? net)
                 (list->array 1 net)
                 net))
            ; new network (an alist)
            (pair
             (if which
                 (cdr (assq #:wnet x))
                 (cdr (assq #:bnet x))))))))))

(define (file-write-net file episode wnet bnet)
  (call-with-output-file file
    (lambda (p)
      (format p "((#:episode . ~a)~%" episode)
      (format p "(#:wnet .~%")
      (write wnet p)
      (format p "~%)~%(#:bnet .~%")
      (write bnet p)
      (format p "))~%"))))

;----

(define (roll-dices)
  (let ((d1 (1+ (truncate (random 6 *rands*))))
        (d2 (1+ (truncate (random 6 *rands*)))))
    (list d1 d2)))

(define (policy-take-action bg net dices)
  (let ((paths (let ((lst (bg-find-all-states bg dices)))
                 (if (bg-ply bg)
                     lst (reverse lst))))
        (bout -999)
        (bpath #f)
        (bvxi (make-typed-array 'f32 *unspecified* 198))
        (vxi (net-vxi net))) ; lend networks-input array
    (loop-for path in paths do
      ;(LLL "  path: ~s~%" path)
      (let ((bg path))
        (set-bg-input bg vxi)
        (net-run net vxi)
        (let ((out (net-vyo net)))
          ; FIX: should we consider white(idx-0) > black(idx-1) ?
          (if (> (array-ref out 0) bout)
              (begin ; keep best-scored
                ;(LLL "  best-net-out: ~s~%" out)
                (set! bout (array-ref out 0))
                (set! bpath path)
                (scopy! vxi bvxi))))))
    (if bpath ; if path found, ie didn't terminate
        (begin
          ; restore best-input to network (ie we keep this future)
          (scopy! bvxi (net-vxi net))
          bpath)
        ; got terminal-state
        #f)))

(define (human-take-action bg dices)
  (let ((paths (bg-find-all-states bg dices))
        (i 0))
    (cond
      ((eq? paths '()) #f) ; no paths to take
      (else
    (loop-for path in paths do
      (format #t "~a path: ~s~%" i path)
      (set! i (1+ i)))
    ; ---- print board ---------
    (do ((p 0 (1+ p)))
        ((>= p 24))
      (format #t "~2d  " p))
    (format #t "~%")
    (let ()
      (do ((p 0 (1+ p)))
          ((>= p 24))
        (let ((pcs 0))
          (loop-for path in paths do
            (let ((x (array-ref (bg-w-pts path) p)))
              (if (and (> x 0)
                       (or (= pcs 0)
                           (< pcs 9)))
                (set! pcs (+ pcs 1)))))
          (if (> pcs 0)
              (format #t "~2d  " pcs)
              (format #t "    ")))))
    (format #t "~%Please select path> ")
    ; --------------------------
    (let ((n (read)))
      (list-ref paths n))))))

(define (style-take-action bg dices style)
  (let ((paths (bg-find-all-states bg dices))
        (i 0))
    (if (= (length paths) 0)
        #f
        (cond
      ((eq? style #:random)
       (list-ref paths (random (length paths))))
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
           (car (last-pair sels)))))))))

(define (state-terminal? bg)
  (or (= (bg-w-rem bg) 15) ; white has won
      (= (bg-b-rem bg) 15))) ; black has won

(define (get-reward bg)
  ; assuming we can make a move, see if the move has put us in an terminal position
  (cond
    ; Until s' is terminal (bg2 is part of s')
    ((state-terminal? bg)
     (let ((ply (bg-ply bg))) ; who's turn it was, and receives the reward
       ; in terminal state, we get a reward of 1
       (assert (= (if (bg-ply bg) (bg-w-rem bg) (bg-b-rem bg)) 15) "get-reward, not terminal!")
       (list 1. #t)))
    (else
     (list 0. #f))))

(define (run-ml-learn bg rl net terminal-state loser)
  ; need to rerun network to get fresh output at each layer
  ; needed by backprop
  (net-run net (net-vxi net)) ; uses the best-path as input
  (let ((Vnew (net-vyo net)))
    (match (get-reward bg)
      ((reward terminal-state)
       ; sane state
       (if loser
           (assert (state-terminal? bg) "loser in non-terminal"))
       (let ((rewarr (make-typed-array 'f32 0. 2)))
         (if (> reward 0)
             (begin
               (array-set! rewarr (if loser 0. 1.) 0)
               (array-set! rewarr (if loser 1. 0.) 1)))
         (run-tderr net rewarr rl terminal-state))))))

(define (run-turn-ml bg net dices)
  (policy-take-action bg net dices))

(define (run-turn-human bg dices)
  (human-take-action bg dices))

(define (run-turn-style bg dices style)
  (style-take-action bg dices style))

(define (run-turn bg net dices)
  (cond
   ((eq? net #:human) ; human type of neural-network controls player
     (run-turn-human bg dices))
   ((eq? net #:late) ; remove pieces as late as possible
     (run-turn-style bg dices #:late))
   ((eq? net #:early) ; remove pieces as soon as possible
     (run-turn-style bg dices #:early))
   ((eq? net #:random) ; make random moves
     (run-turn-style bg dices #:random))
   ((eq? net #:bar) ; try to bar opponents pieces
     (run-turn-style bg dices #:bar))
   ((eq? net #:safe) ; avoid exposed positions
     (run-turn-style bg dices #:safe))
   (else ; player is controlled by artificial type of neural-network
     (run-turn-ml bg net dices))))

(define* (run-tdgammon wnet bnet #:key episodes start-episode save verbose thread)
  ; initialize theta, given by parameters wnet and bnet
  (let* ((gam 0.9) ; td-gamma
        (lam 0.9) ; eligibility-trace decay
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (rlw (make-rl gam lam))
        (rlb (make-rl gam lam))
        (wwin 0) (bwin 0)
        (terminal-state #f)
        (start-time (current-time))
        (totsteps 0))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((and episodes (>= episode episodes)))
      ; save the network now and then
      (if (and save wnet (= (modulo episode 100) 0))
          (file-write-net (format #f "~a-net-~a.txt" thread
                                  (+ (or start-episode 0) episode))
                          (+ (or start-episode 0) episode) wnet bnet))
      ; set s to initial state of episode
      (set! bg (setup-bg))
      (set-bg-ply! bg #t) ; whites turn
      (set! dices (roll-dices))
      (set! terminal-state #f)
      (rl-episode-clear rlw)
      (rl-episode-clear rlb)
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
          ; Set initial Vold
          (if (and (list? wnet) (= step 0))
            (let ((vxi (net-vxi wnet))) ; lend networks-input array
              (set-bg-input bg vxi)
              (net-run wnet vxi)
              (rl-init-step rlw wnet)))
          (if (and (list? bnet) (= step 1))
            (let ((vxi (net-vxi bnet))) ; lend networks-input array
              (set-bg-input bg vxi)
              (net-run bnet vxi)
              (rl-init-step rlb bnet)))
          ; a <- pi(s)  ; set a to action given by policy for s
          ; Take action a, observe r and next state s'
          ;     new state, s', consists of bg2 and new dice-roll
          ;     s =  { bg, dices }
          ;     s' = { best-bg, new-dice-roll }
          (match (run-turn bg (if ply wnet bnet) dices)
            (#f ; player can't move (example is all pieces are on the bar)
              ; since we have no moves to consider/evaluate, we just yield to the other player
             (assert (not (state-terminal? bg)))
             'ok)
            (new-bg
             (set! terminal-state (state-terminal? new-bg))
             (if (array? (if ply wnet bnet)) ; ML-player
                 ; learn winner network
                 (run-ml-learn new-bg
                               (if ply rlw rlb)
                               (if ply wnet bnet)
                               terminal-state #f))
             (if (array? (if ply bnet wnet)) ; ML-player
                 ; if in terminal-state, also let loser learn 
                 (if terminal-state
                   (run-ml-learn new-bg
                                 (if ply rlb rlw)
                                 (if ply bnet wnet)
                                 terminal-state #t)))
             ; evolve state
             ; s <- s'
             (set! bg new-bg)
             (set! totsteps (+ totsteps step))
             (cond
              ((= (bg-w-rem bg) 15)
               (assert terminal-state)
               (set! wwin (+ wwin 1)))
              ((= (bg-b-rem bg) 15)
               (assert terminal-state)
               (set! bwin (+ bwin 1))))
             (if terminal-state
               (format #t "~a.~a.~a s/t:~a winner:~a [~a,~a]~%" thread episode step
                       (inexact->exact (truncate (/ totsteps (- (current-time) start-time -1))))
                       (if (= (bg-w-rem bg) 15) "WHITE" "BLACK")
                       wwin bwin))))
          ; s <- s'
          (set! dices (roll-dices))
          (set-bg-ply! bg (not ply))
          ; bookkeeping
          ; ensure always 15 pieces is present on board+removed+bar
          (let ((wtot 0)
                (btot 0))
            (array-for-each (lambda (x) (set! wtot (+ wtot x))) (bg-w-pts bg))
            (array-for-each (lambda (x) (set! btot (+ btot x))) (bg-b-pts bg))
            (set! wtot (+ wtot (bg-w-rem bg)))
            (set! wtot (+ wtot (bg-w-bar bg)))
            (set! btot (+ btot (bg-b-rem bg)))
            (set! btot (+ btot (bg-b-bar bg)))
            (assert (= wtot 15) (format #f "w-pcs/=15:~a" wtot))
            (assert (= btot 15) (format #f "b-pcs/=15:~a" btot))
            ))))
    (list wwin bwin)))

(define* (run-tdgammon-measure file #:key episodes thread)
  (let* ((bnet (file-load-net file #f))
         (play-random (run-tdgammon #:random bnet #:episodes (or episodes 25) #:start-episode 0 #:save #f #:thread thread))
         (play-early  (run-tdgammon #:early  bnet #:episodes (or episodes 25) #:start-episode 0 #:save #f #:thread thread))
         (play-late   (run-tdgammon #:late   bnet #:episodes (or episodes 25) #:start-episode 0 #:save #f #:thread thread))
         (play-bar    (run-tdgammon #:bar    bnet #:episodes (or episodes 25) #:start-episode 0 #:save #f #:thread thread))
         (play-safe   (run-tdgammon #:safe   bnet #:episodes (or episodes 25) #:start-episode 0 #:save #f #:thread thread))
         (totwwin 0) (totbwin 0))
    ; sum . zip
    (set! totwwin (+ totwwin (car play-random)))
    (set! totbwin (+ totbwin (cadr play-random)))
    (set! totwwin (+ totwwin (car play-early)))
    (set! totbwin (+ totbwin (cadr play-early)))
    (set! totwwin (+ totwwin (car play-late)))
    (set! totbwin (+ totbwin (cadr play-late)))
    (set! totwwin (+ totwwin (car play-bar)))
    (set! totbwin (+ totbwin (cadr play-bar)))
    (set! totwwin (+ totwwin (car play-safe)))
    (set! totbwin (+ totbwin (cadr play-safe)))

    (format #t "RESULT: ~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a~%"
            totwwin totbwin
            (car play-random) (cadr play-random)
            (car play-early)  (cadr play-early)
            (car play-late)   (cadr play-late)
            (car play-bar)   (cadr play-bar)
            (car play-safe)   (cadr play-safe))))
