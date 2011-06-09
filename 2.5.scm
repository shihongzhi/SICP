(define (fast-expt-iter a b n)
  (define (square x) (* x x))
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        ((odd? n) (fast-expt-iter (* a b) b (- n 1)))))

(define (cons a b)
  (* (fast-expt-iter 1 2 a)
     (fast-expt-iter 1 3 b)))

(define (car x)
  (define (remainder-2 t result)
    (if (= 0 (remainder t 2))
        (remainder-2 (/ t 2) (+ result 1))
        result))
  (remainder-2 x 0))

(define (cdr x)
  (define (remainder-3 t result)
    (if (= 0 (remainder t 3))
        (remainder-3 (/ t 3) (+ result 1))
        result))
  (remainder-3 x 0))

(car (cons 3 4))
