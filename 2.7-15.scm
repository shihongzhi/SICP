(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

;2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                  (- (upper-bound x) (lower-bound y))))

(sub-interval (make-interval 1 2) (make-interval -1 1))

;2.9
;加和减就是两个宽度的和，而乘和除就不一定了

;2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (= (upper-bound y) (lower-bound y))
      (display "error!")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))))

(div-interaval (make-interval 2 4) (make-interval 3 3))

;2.11
;分类讨论正负号，共16种，然后去掉非法的5种，就是9种

;2.12
(define (make-center-percent center percent)
  (make-interval (- center (* center percent)) (+ center (* center percent))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))::

(define (percent i)
  (/ (- (upper-bound i) (center i)) (center i)))

(percent (make-center-percent 6.8 0.1))

;2.13
;http://sicp.org.ua/sicp/Exercise2-13


;2.14
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (par1 x y)
  (div-interval (mul-interval x y) (add-interval x y)))
(define (par2 x y)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one x) (div-interval one y)))))
;有下面两个test看出不同的表达式，结果不同
(display (percent (par1 (make-center-percent 10 0.01) (make-center-percent 50 0.02))))
(display (percent (par2 (make-center-percent 10 0.01) (make-center-percent 50 0.02))))

;2.15
;她说的是对的，因为变量的重复出现会影响精度。
;2.16
;
