(define (compose f g)
  (lambda (x) (f (g x))))
;f递归循环n次
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
;f迭代循环n次
;(define (repeated f n)
;  (define (repeated-iter k result)
;    (if (= k 0)
;        result
;        (repeated-iter (- k 1) (f result))))
;  (lambda (x) (repeated-iter n x)))

((repeated (lambda (x) (* x x)) 2) 5)

;1.44
(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
;下面这个是错误的，因为这是对smooth之后的f循环n次
;(define (smooth-repeated f n)
;  (repeated (smooth f) n))
;下面是对f应用smooth循环作用n次
(define (smooth-repeated f n)
  ((repeated smooth n) f))
((smooth-repeated (lambda (x) (* x x)) 2) 5)

;1.45
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;求y^4=x的y值，循环10次，初始猜测为1.0
(define (sqrt-sqrt x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (* y y y)))) 10) 1.0))

(sqrt-sqrt 16)

;1.46
(define (iterative-improve enough? improve)
  (define (iter guess)
    (if (enough? guess)
        guess
        (iter (improve guess))))
  (lambda (x) (iter x)))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess)
  (lambda (x) (average guess (/ x guess))))
