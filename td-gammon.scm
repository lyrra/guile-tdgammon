
(define (file-load-net file which)
  (let ((net #f))
    (call-with-input-file file
      (lambda (p)
        (let ((x (read p)))
          (set! net
                (car (cdr (if which (caddr x) (cadddr x))))))))
    (format #t "loaded network!~%")
    net))

(define (make-net)
  (let ((mhw (rand-m! (make-typed-array 'f32 *unspecified* 40 198)))
        (vhz (rand-v! (make-typed-array 'f32 *unspecified* 40)))
        (vho (rand-v! (make-typed-array 'f32 *unspecified* 40)))
        (myw (rand-m! (make-typed-array 'f32 *unspecified* 2 40)))
        (vyz (rand-v! (make-typed-array 'f32 *unspecified* 2)))
        (vyo (rand-v! (make-typed-array 'f32 *unspecified* 2)))
        (vxi (make-typed-array 'f32 *unspecified* 198)))
    (list mhw vhz vho myw vyz vyo vxi)))

(define (net-vyo net) (list-ref net 5))
(define (net-vxi net) (list-ref net 6))

(define (sigmoid z)
  (/ 1. (+ 1. (exp (- z)))))

(define (sigmoid-grad z)
  (let ((a (sigmoid z)))
    (* a (- 1 a))))

; Dsigmoid(x) = sigmoid(x) (1 - sigmoid(x))
(define (array-sigmoid src dst)
  (array-map! dst (lambda (z) (sigmoid z))
              src))

; calculate gradient GRAD(weight, output)
(define (set-sigmoid-gradient! grad netz)
  (array-map! grad (lambda (z) (sigmoid-grad z))
                   netz))

; gradient-descent, return weight update in grads
(define (update-eligibility-traces net eligs)
  (match eligs
    ((emhw0 emhw1 emyw0)
  (match net
    ((mhw vhz vho myw vyz vyo vxi)
     (let ((go  (make-typed-array 'f32 0.  2))
           (gho (make-typed-array 'f32 0. 2 40)))
       (set-sigmoid-gradient! go vyz)
       (match (array-dimensions myw)
         ((r c)
           (do ((i 0 (+ i 1))) ((= i r)) ; i = each output neuron
             (let ((g (array-ref go i)))
               (do ((j 0 (+ j 1))) ((= j c)) ; j = each hidden output
                 (let* ((o (array-ref vho j))
                        (w (array-ref myw i j))
                        (e (array-ref emyw0 i j)))
              (if (or (> (* g o) 10) (< (* g o) -10)) ; absurd
                  (begin
                   (format #t "emyw0: absurd elig update> e=~f (~a * ~a)~%" (* g o) g o)
                  (exit)))
                   (array-set! emyw0 (+ e (* g o)) i j)
                   (array-set! gho (+ (array-ref gho i j) (* g w)) i j)))))))

       ; gradient through hidden-ouput sigmoid
       ; FIX: make set-sigmoid-gradient! general enough
       (match (array-dimensions myw)
         ((r c)
          (do ((i 0 (+ i 1))) ((= i r)) ; i = each output neuron
          (do ((j 0 (+ j 1))) ((= j c)) ; j = each hidden output
            (let ((g (array-ref gho i j))
                  (z (array-ref vhz j)))
              (array-set! gho (* g (sigmoid-grad z)) i j))))))

       (match (array-dimensions mhw)
         ((r c)
           (do ((k 0 (+ k 1))) ((= k 2)) ; i = each output neuron
             (do ((i 0 (+ i 1))) ((= i r)) ; i = each hidden neuron
               (do ((j 0 (+ j 1))) ((= j c)) ; j = each network-input
                 (let* ((g (array-ref gho k i))
                        (x (array-ref vxi j))
                        (ev (if (= k 0) emhw0 emhw1))
                        (e (array-ref ev i j)))
              (if (or (> (* g x) 10) (< (* g x) -10)) ; absurd
                  (begin
                   (format #t "emhw0/1: absurd elig update> e=~f (~a * ~a)~%" (* g x) g x)
                  (exit)))
                   (array-set! ev (+ e (* g x)) i j)))))))))))))

; gradient-descent, return weight update in grads
(define (update-weights net alpha tderr eligs)
  (match eligs
    ((emhw0 emhw1 emyw0)
  (match net
    ((mhw vhz vho myw vyz vyo vxi)
     ;----------------------------------------
     (match (array-dimensions myw)
       ((r c)
        (do ((i 0 (+ i 1))) ((= i r)) ; i = each output neuron
          (let ((tde (array-ref tderr i)))
          (do ((j 0 (+ j 1))) ((= j c)) ; j = each hidden output
            (let ((w (array-ref myw i j))
                  (e (array-ref emyw0 i j)))
              (array-set! myw (+ w (* alpha e tde)) i j)
              (if (or (> w 10) (< w -10)) ; absurd
                  (begin
                   (format #t "absurd weight update> w=~f, e=~f~%" w e)
                  (exit)))))))))
     ; propagate gradient backwards to hidden weights
     (match (array-dimensions mhw)
       ((r c)
        (do ((i 0 (+ i 1))) ((= i r)) ; i = each hidden neuron
          (do ((j 0 (+ j 1))) ((= j c)) ; j = each network-input
            (let ((w (array-ref mhw i j))
                  (e (+ (* (array-ref tderr 0) (array-ref emhw0 i j))
                        (* (array-ref tderr 0) (array-ref emhw0 i j)))))
              (array-set! mhw (+ w (* alpha e)) i j)))))))))))

(define (net-run net input)
  (match net
    ((mhw vhz vho myw vyz vyo vxi)
     (sgemv! 1. mhw CblasNoTrans input 0. vhz)
     (array-sigmoid vhz vho)
     (sgemv! 1. myw CblasNoTrans vho 0. vyz)
     (array-sigmoid vyz vyo)
     #f)))

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
        (vxi (make-typed-array 'f32 *unspecified* 198)))
    (loop-for path in paths do
      ;(format #t "  path: ~s~%" path)
      (let ((bg path))
        (set-bg-input bg vxi)
        (net-run net vxi)
        (let ((out (net-vyo net)))
          ; FIX: should we consider white(idx-0) > black(idx-1) ?
          (if (> (array-ref out 0) bout)
              (begin
                ;(format #t "  best-net-out: ~s~%" out)
                (set! bout (array-ref out 0))
                (set! bpath path)
                (array-map! bvxi (lambda (x) x) vxi))))))
    (if bpath ; if not terminate
        (begin
          ; restore best-output to network (ie we keep this future)
          (array-map! (net-vxi net) (lambda (x) x) bvxi)
          bpath)
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

; Vold is the previous state-value, V(s), and Vnew is the next state-value, V(s')
(define (run-tderr net Vold reward eligs gam lam terminal-state)
  (let ((Vnew (net-vyo net))
        (alpha 0.01)
        (tderr (make-typed-array 'f32 0. 2))
        (vxi (net-vxi net)))

    ;---------------------------------------------
    (cond
     (terminal-state
      (sv-! tderr reward Vold)) ; reward - V(s)
     (else
      ; tderr <- r + gamma * V(s') - V(s)
      (svvs*! tderr Vnew gam) ; gamma * V(s')
      (sv-! tderr tderr Vold) ; gamma * V(s') - V(s)
      (array-map! tderr (lambda (x r) (+ x r)) tderr reward)))

    ;---------------------------------------------
    ; update network weights
    ; delta to update weights: w += alpha * tderr * elig
    ; where elig contains diminished gradients of network activity
    ; AEG: alpha * tderr * eligs
    (update-weights net alpha tderr eligs)

    ;---------------------------------------------
    ; discount eligibility traces
    ; update eligibility traces
    ; elig  <- gamma*lambda * elig + Grad_theta(V(s))
    ; z <- y*L* + Grad[V(s,w)]
    (loop-for elig in eligs do
      (array-map! elig (lambda (e) (* e lam)) elig))
    (update-eligibility-traces net eligs)))

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
       (match rl
         ((Vold eligs gam lam)
          (let ((rewarr (make-typed-array 'f32 0. 2)))
            (if (> reward 0)
              (begin
                (array-set! rewarr (if loser 0. 1.) 0)
                (array-set! rewarr (if loser 1. 0.) 1)))
            (run-tderr net Vold rewarr eligs gam lam terminal-state)
            ; update caches
            (array-map! Vold (lambda (x) x) Vnew))))))))

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

(define (file-write-net file episode wnet bnet)
  (call-with-output-file file
    (lambda (p)
      (format p "(#:episode ~a~%" episode)
      (format p "(#:wnet~%")
      (write wnet p)
      (format p "~%)~%(#:bnet~%")
      (write bnet p)
      (format p "))~%")
      )))

; [Vold, eligs, gam, lam]
(define (make-rl gam lam)
  (list (make-typed-array 'f32 0. 2) ; Vold
        ; eligibility traces, 0-1 is index in output-layer
        (list (make-typed-array 'f32 *unspecified* 40 198) ; mhw-0
              (make-typed-array 'f32 *unspecified* 40 198) ; mhw-1
              (make-typed-array 'f32 *unspecified* 2 40))  ; myw-0
        gam lam))

(define (rl-episode-clear rl)
  ; initialize eligibily traces to 0
  (match rl
    ((Vold eligs gam lam)
     (loop-for arr in eligs do
       (array-map! arr (lambda (x) 0.) arr)))))

(define* (run-tdgammon wnet bnet #:key episodes save)
  ; initialize theta, given by parameters wnet and bnet
  (let* ((gam 0.9) ; td-gamma
        (lam 0.9) ; eligibility-trace decay
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (rlw (make-rl gam lam))
        (rlb (make-rl gam lam))
        (wwin 0) (bwin 0)
        (terminal-state #f))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        ((and episodes (>= episode episodes)))
      ; save the network now and then
      (if (and save wnet (= (modulo episode 100) 0))
          (file-write-net (format #f "net-~a.txt" episode)
                          episode wnet bnet))
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
          (format #t "~a.~a: [~a/~a] dices: ~s w/b-turn: ~a bar:[~a,~a] rem:[~a,~a]~%"
                  episode step wwin bwin dices (bg-ply bg)
                  (bg-w-bar bg) (bg-b-bar bg)
                  (bg-w-rem bg) (bg-b-rem bg))
          (bg-print-board bg)
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
             (if (list? (if ply wnet bnet)) ; ML-player
                 ; learn winner network
                 (run-ml-learn new-bg
                               (if ply rlw rlb)
                               (if ply wnet bnet)
                               terminal-state #f))
             (if (list? (if ply bnet wnet)) ; ML-player
                 ; if in terminal-state, also let loser learn 
                 (if terminal-state
                   (run-ml-learn new-bg
                                 (if ply rlb rlw)
                                 (if ply bnet wnet)
                                 terminal-state #t)))
             ; evolve state
             ; s <- s'
             (set! bg new-bg)
             (cond
              ((= (bg-w-rem bg) 15)
               (set! wwin (+ wwin 1))
               (format #t "### WHITE HAS WON!~%-----------------------------------~%"))
              ((= (bg-b-rem bg) 15)
               (set! bwin (+ bwin 1))
               (format #t "OOO BLACK HAS WON!~%-----------------------------------~%")))))
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

(define* (run-tdgammon-measure file #:key episodes)
  (let* ((bnet (file-load-net file #f))
         (play-random (run-tdgammon #:random bnet #:episodes (or episodes 25) #:save #f))
         (play-early  (run-tdgammon #:early  bnet #:episodes (or episodes 25) #:save #f))
         (play-late   (run-tdgammon #:late   bnet #:episodes (or episodes 25) #:save #f))
         (play-bar    (run-tdgammon #:bar    bnet #:episodes (or episodes 25) #:save #f))
         (totwwin 0) (totbwin 0))
    ; sum . zip
    (set! totwwin (+ totwwin (car play-random)))
    (set! totbwin (+ totbwin (cadr play-random)))
    (set! totwwin (+ totwwin (car play-early)))
    (set! totbwin (+ totbwin (cadr play-early)))
    (set! totwwin (+ totwwin (car play-late)))
    (set! totbwin (+ totbwin (cadr play-late)))
    (format #t "RESULT: ~a,~a,~a,~a,~a,~a,~a,~a,~a,~a~%"
            totwwin totbwin
            (car play-random) (cadr play-random)
            (car play-early)  (cadr play-early)
            (car play-late)   (cadr play-late)
            (car play-bar)   (cadr play-bar))))
