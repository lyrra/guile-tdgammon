(define (test-backgammon-td-run conf)
  (init-rand 111)
  (let ((net (make-net #:in 198
                       #:out (get-conf conf 'numout)
                       #:hid (get-conf conf 'numhid)
                       #:wdelta (get-conf conf 'waccu))))
    (run-episodic-selfplay tdgammon-run-episode net net conf)
    net))

(define (test-get-net-arrs-sum net)
  (let ((sum 0))
    (array-for-each
     (lambda (rv)
       (gpu-refresh-host rv)
       (array-for-each
        (lambda (x)
          (set! sum (+ sum x)))
        (gpu-array rv)))
     (netr-arrs net))
    sum))

(define-test (test-backgammon-td-run-out1)
  (let ((net (test-backgammon-td-run
              '((numout . 1)
                (numhid . 13)
                (save . #f)
                (measure . #f)
                (episodes . 1)
                ))))
    (let ((sum (test-get-net-arrs-sum net)))
      (test-assert (< (abs (- sum 20.29459895)) 0.0001) "net-arrs-sum diff-error"))))

(define-test (test-backgammon-td-run-out2)
  (let ((net (test-backgammon-td-run
              '((numout . 2)
                (numhid . 13)
                (save . #f)
                (measure . #f)
                (episodes . 1)
                ))))
    (let ((sum (test-get-net-arrs-sum net)))
      (test-assert (< (abs (- sum 22.46425635)) 0.0001) "net-arrs-sum diff-error"))))

