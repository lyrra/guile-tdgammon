
(define (file-load-net file)
  (let ((net #f))
    (call-with-input-file file
      (lambda (p)
        (let ((x (read p)))
          (match (car x)
            (#:episode ;old network type
             (error "old-network, not supported anymore")
             (set! net
                   (car (cdr (caddr x))))
             ; if net is saved as a list, convert to vector
             (net-make-from
              (if (list? net)
                  (list->array 1 net)
                  net)))
            ; new network (an alist)
            (pair
             (net-make-from (cdr (assq #:wnet x))))))))))

(define (file-write-net file episode net)
  (call-with-output-file file
    (lambda (p)
      (format p "((#:episode . ~a)~%" episode)
      (format p "(#:wnet .~%")
      (write (net-serialize net) p)
      (format p "~%)~%")
      (format p ")~%"))))

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
          (if (> (- (array-ref out 0) (array-ref out 1)) bout)
              (begin ; keep best-scored
                ;(LLL "  best-net-out: ~s~%" out)
                (set! bout (- (array-ref out 0) (array-ref out 1)))
                (set! bpath path)
                (array-scopy! vxi bvxi))))))
    (if bpath ; if path found, ie didn't terminate
        (begin
          ; restore best-input to network (ie we keep this future)
          (net-set-input net bvxi)
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
           (car (last-pair sels)))))
      ((procedure? style) (style bg paths))))))

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

(define (run-ml-learn bg rl net terminal-state loser-input)
  ; need to rerun network to get fresh output at each layer
  ; needed by backprop
  (net-run net (or loser-input (net-vxi net))) ; uses the best-path as input
  (match (get-reward bg)
    ((reward terminal-state)
     ; sane state
     (if loser-input
         (assert (state-terminal? bg) "loser in non-terminal"))
     (let ((rewarr (make-typed-array 'f32 0. 2)))
       (if (> reward 0)
           (begin
             (array-set! rewarr (if loser-input 0. 1.) 0)
             (array-set! rewarr (if loser-input 1. 0.) 1)))
       (run-tderr net rewarr rl terminal-state)))))

(define (run-turn bg net dices)
  (cond
   ((array? net) ; player is controlled by artificial type of neural-network
     (policy-take-action bg net dices))
   ((eq? net #:human) ; human type of neural-network controls player
     (human-take-action bg dices))
   (else
     (style-take-action bg dices net))))

(define* (run-tdgammon net oppo opts #:key episodes start-episode save verbose thread threadio
                       measure)
  ; initialize theta, given by parameters net
  (let* ((gam (get-opt opts 'rl-gam)) ; td-gamma
         (lam (get-opt opts 'rl-lam)) ; eligibility-trace decay
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (rlw (if (not measure) (make-rl gam lam net) #f))
        (rlb (if (and (not measure) (eq? oppo #:self)) (make-rl gam lam net) #f))
        (wwin 0) (bwin 0)
        (terminal-state #f)
        (start-time (current-time))
        (totsteps 0)
        ; need the previous players input (used at terminal state)
        (ovxi (make-typed-array 'f32 *unspecified* 198)))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((and episodes (>= episode episodes)))
      ; merge white and black networks
      ; save the network now and then
      (if (and (not threadio) save (> episode 0) (= (modulo episode 100) 0))
          (file-write-net (format #f "~a-net-~a.txt" thread
                                  (+ (or start-episode 0) episode))
                          (+ (or start-episode 0) episode) net))
      ; set s to initial state of episode
      (set! bg (setup-bg))
      (set-bg-ply! bg #t) ; whites turn
      (set! dices (roll-dices))
      (set! terminal-state #f)
      (if rlw (rl-episode-clear rlw))
      (if rlb (rl-episode-clear rlb))
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
          (if (and rlw (= step 0))
            (let ((vxi (net-vxi net))) ; lend networks-input array
              (rl-episode-clear rlw)
              (set-bg-input bg vxi)
              (net-run net vxi)
              (rl-init-step rlw net)))
          (if (and rlb (= step 1))
            (let ((vxi (net-vxi net))) ; lend networks-input array
              (rl-episode-clear rlb)
              (set-bg-input bg vxi)
              (net-run net vxi)
              (rl-init-step rlb net)))
          ; a <- pi(s)  ; set a to action given by policy for s
          ; Take action a, observe r and next state s'
          ;     new state, s', consists of bg2 and new dice-roll
          ;     s =  { bg, dices }
          ;     s' = { best-bg, new-dice-roll }
          (match (run-turn bg (if (eq? oppo #:self) net (if ply net oppo)) dices)
            (#f ; player can't move (example is all pieces are on the bar)
              ; since we have no moves to consider/evaluate, we just yield to the other player
             (assert (not (state-terminal? bg)))
             'ok)
            (new-bg
             (set! terminal-state (state-terminal? new-bg))
             (if (if ply rlw rlb) ; let ML-player learn the step
               (run-ml-learn new-bg
                             (if ply rlw rlb)
                             net
                             terminal-state #f))
             ; if in terminal-state, also learn the loser experience
             (if (and terminal-state (if ply rlb rlw)) ; ML-player
               (run-ml-learn new-bg
                             (if ply rlb rlw) ; use the previous turns player
                             net
                             terminal-state ovxi))
             ; evolve state
             (array-scopy! (net-vxi net) ovxi)
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
            )))
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
