;递归乘法
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) 
         (product term (next a) next b))))
;迭代乘法
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (square x) (* x x))
(define (next x) (+ x 1))

;36
(product square 1 next 3)
;576
(product-iter square 1 next 4)

;factorial
(define (factorial n)
  (define (identity x) x)
  (product identity 1 next n))
;24
(factorial 4)
;下面用4.0的话就可以把原来是显示成分数的结果显示成小数了
(define (pi n)
  (define (next-2 x) (+ x 2))
  (define (part x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
  (* 4.0 (product part 2 next-2 (* n 2))))

(pi 1000)
