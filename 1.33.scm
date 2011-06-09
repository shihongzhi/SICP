(define (filtered-accumulate filters combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filters a b)
          (combiner (term a) 
                    (filtered-accumulate filters combiner null-value term (next a) next b))
          (filtered-accumulate filters combiner null-value term (next a) next b))))
(define (filtered-accumulate-iter filters combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filters a b)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))


(define (combiner-add a b)
  (+ a b))
(define (combiner-mult a b)
  (* a b))
(define (next-1 x) (+ x 1))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (square x) (* x x))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n last)
  (= n (smallest-divisor n)))
(define (identity x) x)
;18 求1-10中所有素数平方之和
(filtered-accumulate prime? combiner-add 0 identity 1 next-1 10)

(define (gcd-filter i n)
  (and (< i n) (= (gcd n i) 1)))
;189 小于n的所有与n互素的正整数的乘积
(filtered-accumulate gcd-filter combiner-mult 1 identity 1 next-1 10)
;189
(filtered-accumulate-iter gcd-filter combiner-mult 1 identity 1 next-1 10)
