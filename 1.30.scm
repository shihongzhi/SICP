(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (square x) (* x x))
(define (next x) (+ x 1))

(sum square 0 next 3)
