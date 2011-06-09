(define (cont-frac-iter n d k)
  (define (try guess t)
    (let ((next (/ (n t) (+ (d t) guess))))
      (if (= t 0)
          next
          (try next (- t 1)))))
  (try (/ (n k) (d k)) (- k 1)))

(define (cont-frac n d k)
  (define (try t)
    (if (= t k)
        (/ (n t) (d k))
        (/ (n t) (+ (d t) (try (+ t 1))))))
  (try 1))
;抽象combiner
(define (cont-frac-accumulate combiner n d k)
  (define (try guess t)
    (let ((next (/ (n t) (combiner (d t) guess))))
      (if (= t 0)
          next
          (try next (- t 1)))))
  (try (/ (n k) (d k)) (- k 1)))

;0.6180339887498948
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

;1.38
(define (d-e x)
  (if (= (remainder (+ x 1) 3) 0)
      (* 2.0 (/ (+ x 1) 3))
      1))
(define (e k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0) d-e k)))
;算出来的e误差很大啊 等于2.58197670
(e 1000)

;1.39

(define (tan-cf x k)
  (define (n-tan k)
    (if (= k 1)
        x
        (- (* x x)))) ;之前想着抽象cont-frac中的combiner，没想到直接加一个负号
  (define (d-tan k)
    (- (* k 2) 1))
  (cont-frac n-tan d-tan k))

(tan-cf (/ 3.14159 3) 10)

