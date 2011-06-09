(define (inc x)
  (+ x 1))
(define (double f)
  (lambda (x) (f (f x))))
((double inc) 8)
(((double (double double)) inc) 5)
;上式相当与下式,即执行了2^n次，n为double的个数
((double (double (double (double inc)))) 5)
