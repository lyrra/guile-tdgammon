
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
        (vho (rand-v! (make-typed-array 'f32 *unspecified* 40)))
        (myw (rand-m! (make-typed-array 'f32 *unspecified* 2 40)))
        (vyo (rand-v! (make-typed-array 'f32 *unspecified* 2))))
    (list mhw vho myw vyo)))

; Dsigmoid(x) = sigmoid(x) (1 - sigmoid(x))
(define (sigmoid vyo)
  (array-map! vyo (lambda (x)
                    (/ 1. (+ 1. (exp (- x)))))
              vyo))

; gradient-descent
; calculate gradient GRAD(weight, output)
(define (set-sigmoid-gradient! grad out)
  (array-map! grad (lambda (x)
                     (let ((s (/ 1. (+ 1. (exp (- x))))))
                       (* s (- 1. s))))
                   grad))

(define (gradient-descent vxi net grad alpha)
  (match net
    ((mhw vho myw vyo)
     ;  mhw: (40 198)
     ;  vho: (40)
     ;  myw: (2 40)
     ;  vyo: (2) #f32(0.495709627866745 0.46157199144363403)
     ;  grad: (2) #f32(0.25 0.25)

     ; propagate gradient backwards to output weights
     (let ((go (make-typed-array 'f32 0. 40)))
       (match (array-dimensions myw)
         ((r c)
           (do ((i 0 (+ i 1))) ((= i r))
             (let ((g (array-ref grad i)))
               (do ((j 0 (+ j 1))) ((= j c))
                 (let ((w (array-ref myw i j))
                       (o (array-ref vho j)))
                   (array-set! myw (+ (array-ref myw i j) (* alpha o g)) i j)
                   ; populate gradient
                   (array-set! go (+ (array-ref go i) (* g w)) i)
                   ; each of the 40 neurons (i) in hidden layer, is connected to both neurons at output layer
                   ; therefore foreach neuron, we sum the gradient coming from the two neurons below
                   ))))))
       (sigmoid go)
       ; propagate gradient backwards to hidden weights
       (match (array-dimensions mhw)
         ((r c)
           (do ((i 0 (+ i 1))) ((= i r))
             (let ((g (array-ref go i)))
               (do ((j 0 (+ j 1))) ((= j c))
                 (let ((w (array-ref mhw i j))
                       (o (array-ref vxi j)))
                   (array-set! mhw (+ (array-ref mhw i j) (* alpha o g)) i j)))))))))))

(define (net-run net input)
  (match net
    ((mhw vho myw vyo)
     (sgemv! 1. mhw CblasNoTrans input 0. vho)
     (sigmoid vho)
     (sgemv! 1. myw CblasNoTrans vho 0. vyo)
     (sigmoid vyo)
     #f)))

(define (roll-dices)
  (let ((d1 (1+ (truncate (random 6 *rands*))))
        (d2 (1+ (truncate (random 6 *rands*)))))
    (list d1 d2)))

(define (best-path paths net out-idx)
  (let ((bout -999)
        (bpath #f)
        (bvxi (make-typed-array 'f32 *unspecified* 198))
        (vxi (make-typed-array 'f32 *unspecified* 198)))
    (loop-for path in paths do
      ;(format #t "  path: ~s~%" path)
      (let ((bg path))
        (set-bg-input bg vxi #t)
        (net-run net vxi)
        (let ((out (cadddr net)))
          ; FIX: should we consider white(idx-0) > black(idx-1) ?
          (if (> (array-ref out out-idx) bout) ; 0 is white's chance of winning, 1 is black
              (begin
                ;(format #t "  best-net-out: ~s~%" out)
                (set! bout (array-ref out out-idx))
                (set! bpath path)
                (array-map! bvxi (lambda (x) x) vxi))))))
    (if bpath ; if not terminate
        (list bvxi bout bpath)
        #f)))

(define (policy-take-action bg net dices)
  (let ((paths (bg-find-all-states bg dices)))
    (best-path paths net (if (bg-ply bg) 0 1))))

(define (human-take-action bg dices)
  (let ((paths (bg-find-all-states bg dices))
        (i 0))
    (cond
      ((not (eq paths '())) '()) ; no paths to take
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
       (car (last-pair paths)))))))

(define (state-terminal? bg)
  (or (= (bg-w-rem bg) 15) ; white has won
      (= (bg-b-rem bg) 15))) ; black has won

(define (get-reward bg)
  ; we can make a move, see if the move has put us in an terminal position
  (cond
    ; Until s' is terminal (bg2 is part of s')
    ((state-terminal? bg)
     (let ((ply (bg-ply bg))) ; who's turn it was, and receives the reward
       ; in terminal state, we get a reward of 1
       (assert (= (if (bg-ply bg) (bg-w-rem bg) (bg-b-rem bg)) 15))
       (list 1. #t)))
    (else
     (list 0. #f))))

(define (run-tderr vxi net tderr Vold Vnew grad elig reward gamma gamma-lambda)
  (let ((alpha 0.01))
    ; calculate gradient GRAD(weight, output)
    (set-sigmoid-gradient! grad Vnew)

    ;---------------------------------------------
    ; update eligibility traces
    ; elig  <- gamma_lambda * e + Grad_theta(V(s))
    ; z <- y*L* + Grad[V(s,w)]
    ; FIX: elig för varje vikt i nätet? behöver vi vxi?
    (array-map! elig (lambda (e g)
                       (+ (* gamma-lambda e) g))
                elig grad)

    ;---------------------------------------------
    ; tderr <- r + gamma * V(s') - V(s)
    (svvs*! tderr Vnew gamma)
    (sv-! tderr tderr Vold)
    (array-map! tderr (lambda (x r) (+ x r)) tderr reward)

    ; delta to update weights: w += alpha * tderr * grad * elig
    (array-map! tderr (lambda (t g e) (* t g e)) tderr grad elig)

    ;---------------------------------------------
    ; update network weights
    ; theta <- theta + alpha * tderr * elig
    (gradient-descent vxi net tderr alpha)))

(define (run-mcerr vxi net tderr Vold Vnew grad reward gamma gamma-lambda)
  (let ((alpha 0.01))
    ; calculate gradient GRAD(weight, output)
    (set-sigmoid-gradient! grad Vnew)

    ;---------------------------------------------
    ; tderr <- r + gamma * V(s') - V(s)
    (svvs*! tderr Vnew gamma)
    (sv-! tderr tderr Vold)
    (array-map! tderr (lambda (x r) (+ x r)) tderr reward)

    ; delta to update weights: w += alpha * tderr * grad
    (array-map! tderr (lambda (t g) (* t g)) tderr grad)

    ;---------------------------------------------
    ; update network weights
    ; theta <- theta + alpha * tderr
    (gradient-descent vxi net tderr alpha)))

(define (learn-net net es tderr grad gamma lam)
  (let ((n 1))
    (loop-for e in es do
      (match e
        ((rewarr vxi Vold Vnew)
         (run-mcerr vxi net tderr Vold Vnew grad rewarr gamma (expt lam n))
         (set! n (1+ n)))))))

(define (run-turn-ml bg net dices tderr Vold grad elig es gamma gamma-lambda)
  (let ((ply (bg-ply bg)))
    ;(format #t "  run-turn ply=~a, dices=~s~%" ply dices)
    (match (policy-take-action bg net dices)
      (#f ; player can't move (example is all pieces are on the bar)
       ; since we have no moves to consider/evaluate, we just yield to the other player
       ;(format #t "  player (~a) cant move!~%" (bg-ply bg))
       #f)
      ((vxi best-out best-path)
       (let ((bg2 best-path)
             (Vnew (cadddr net)))
         (set-bg-input bg vxi #t) ; net-output is stale, refresh it
         (net-run net vxi)
         (match (get-reward bg2)
           ((reward terminal-state)
            (let ((rewarr (make-typed-array 'f32 0. 2)))
              (if (> reward 0)
                (begin
                  (array-set! rewarr 1. (if ply 0 1))
                  (array-set! rewarr -1. (if ply 1 0))))
              ; Vold is the previous state-value, V(s), and Vnew is the next state-value, V(s')
              (set! es (cons (list rewarr vxi Vold Vnew) es))
              ;(run-tderr vxi net tderr Vold Vnew grad elig rewarr gamma gamma-lambda)
              )
            ; caches
            (array-map! Vold (lambda (x) x) Vnew)
            (list bg2 terminal-state es))))))))

(define (run-turn-human bg dices)
  (let ((ply (bg-ply bg)))
    (match (human-take-action bg dices)
      (#f ; player can't move (example is all pieces are on the bar)
       ; since we have no moves to consider/evaluate, we just yield to the other player
       #f)
      (new-bg
        (list new-bg (state-terminal? new-bg) '())))))

(define (run-turn-style bg dices style)
  (let ((ply (bg-ply bg)))
    (match (style-take-action bg dices style)
      (#f ; player can't move (example is all pieces are on the bar)
       ; since we have no moves to consider/evaluate, we just yield to the other player
       #f)
      (new-bg
        (list new-bg (state-terminal? new-bg) '())))))

(define (run-turn bg net dices tderr Vold grad elig es gamma gamma-lambda)
  (let ((ply (bg-ply bg)))
    (cond
     ((eq? net #:human) ; human type of neural-network controls player
       (run-turn-human bg dices))
     ((eq? net #:late) ; remove pieces as late as possible
       (run-turn-style bg dices #:late))
     ((eq? net #:early) ; remove pieces as soon as possible
       (run-turn-style bg dices #:early))
     ((eq? net #:random) ; make random moves
       (run-turn-style bg dices #:random))
     (else ; player is controlled by artificial type of neural-network
       (run-turn-ml bg net dices tderr Vold grad elig es gamma gamma-lambda)))))

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

(define* (run-tdgammon wnet bnet #:key episodes save)
  ; initialize theta, given by parameters wnet and bnet
  (let ((gamma 0.9) ; td-gamma
        (gamma-lambda 0.9) ; eligibility-trace decay
        (bg (setup-bg))
        (dices (roll-dices))
        ; eligibility-traces
        (wes '())
        (bes '())
        (welig (make-typed-array 'f32 *unspecified* 2))
        (belig (make-typed-array 'f32 *unspecified* 2))
        (wwin 0) (bwin 0)
        (terminal-state #f)
        (episodes-done #f))
    ; loop for each episode
    (do ((episode 0 (1+ episode)))
        (episodes-done)
        ;((= episode 10))
      (if (>= episode episodes) (set! episodes-done #t))
      ; save the network now and then
      (if (and save wnet (= (modulo episode 20) 0))
          (file-write-net (format #f "net-~a.txt" episode)
                          episode wnet bnet))
      (let ((wvyo (make-typed-array 'f32 0. 2))
            (bvyo (make-typed-array 'f32 0. 2))
            (wgrad (make-typed-array 'f32 0. 2))
            (bgrad (make-typed-array 'f32 0. 2))
            (tderr (make-typed-array 'f32 0. 2)))
      ; initialize eligibily traces to 0
      (array-map! welig (lambda (x) 0.) welig)
      (array-map! belig (lambda (x) 0.) belig)
      ; set s to initial state of episode
      (set! bg (setup-bg))
      (set-bg-ply! bg #t) ; whites turn
      (set! dices (roll-dices))
      (set! terminal-state #f)
      (set! wes '())
      (set! bes '())
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
          (match (run-turn bg (if ply wnet bnet)
                           dices tderr
                           (if ply wvyo bvyo)
                           (if ply wgrad bgrad)
                           (if ply welig belig)
                           (if ply wes bes)
                           gamma gamma-lambda)
            (#f 'ok) ; cant move
            ((new-bg is-terminal-state new-w/b-es)
             ; evolve state
             ; s <- s'
             (set! bg new-bg)
             (set! terminal-state is-terminal-state)
             (if is-terminal-state
               (cond
                ((= (bg-w-rem bg) 15)
                 (set! wwin (+ wwin 1))
                 ; at terminal-state go through all-steps and learn
                 (if (list? wnet)
                   (learn-net wnet new-w/b-es tderr wgrad gamma gamma-lambda))
                 (format #t "### WHITE HAS WON!~%-----------------------------------~%")
                 )
                ((= (bg-b-rem bg) 15)
                 (set! bwin (+ bwin 1))
                 ; at terminal-state go through all-steps and learn
                 (if (list? bnet)
                   (learn-net bnet new-w/b-es tderr bgrad gamma gamma-lambda))
                 (format #t "OOO BLACK HAS WON!~%-----------------------------------~%")))
               (if ply (set! wes new-w/b-es) (set! bes new-w/b-es)))))
          (set! dices (roll-dices)) ; also part of state
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
            )))))
    (list wwin bwin)))

(define* (run-tdgammon-measure file #:key episodes)
  (let* ((bnet (file-load-net file #f))
         (play-random (run-tdgammon #:random bnet #:episodes (or episodes 100) #:save #f))
         (play-early  (run-tdgammon #:early  bnet #:episodes (or episodes 100) #:save #f))
         (play-late   (run-tdgammon #:late   bnet #:episodes (or episodes 100) #:save #f))
         (totwwin 0) (totbwin 0))
    ; sum . zip
    (set! totwwin (+ totwwin (car play-random)))
    (set! totbwin (+ totbwin (cadr play-random)))
    (set! totwwin (+ totwwin (car play-early)))
    (set! totbwin (+ totbwin (cadr play-early)))
    (set! totwwin (+ totwwin (car play-late)))
    (set! totbwin (+ totbwin (cadr play-late)))
    (format #t "RESULT: ~a,~a~%" totwwin totbwin)))
