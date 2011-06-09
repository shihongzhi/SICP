(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term-f k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (term-f k))
          ((even? k) (* 2 (term-f k)))
          (else (* 4 (term-f k)))))
  (define (next x) (+ x 1))
  (* h (/(sum term 0 next n) 3)))

(define (cube x) (* x x x))

;1/4
(simpson cube 0 1 1000)
;1/4
(simpson cube 0 1 1000)

