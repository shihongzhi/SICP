(define (fast-expt-iter a b n)
  (define (square x) (* x x))
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        ((odd? n) (fast-expt-iter (* a b) b (- n 1)))))
(fast-expt-iter 1 2 10)