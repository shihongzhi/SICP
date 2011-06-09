(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


(define (sum term a next b)
  (define (add a b) (+ a b))
  (accumulate add term a next b))
(define (prodect term a next b)
  (define (mult a b) (* a b))
  (accumulate mult term a next b))

(define (combiner-add a b)
  (+ a b))
(define (combiner-mult a b)
  (* a b))
(define (next-1 x) (+ x 1))
(define (square x) (* x x))
(accumulate combiner-mult 1 square 1 next-1 4)
(accumulate-iter combiner-mult 1 square 1 next-1 5)
