(define (halve x) (/ x 2))
(define (double x) (+ x x))

;递归
(define (fast-add b n)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-add b (halve n))))
        (else (+ b (fast-add b (- n 1))))))
;迭代
(define (fast-add-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-add-iter a (double b) (halve n)))
        (else (fast-add-iter (+ a b) b (- n 1)))))
(fast-add 3 7)
(fast-add-iter 0 5 9)
