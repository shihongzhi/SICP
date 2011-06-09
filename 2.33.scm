(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;(define (append seq1 seq2)
;  (accumulate cons seq2 seq1))
;(define (length sequence)
;  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(define (square x)
  (* x x))

(map square (list 1 2 3 4 5))
(append (list 1 2 3) (list 2 3 4))
(length (list 1 2 3 4))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;2.35
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 1 2 (list 3 4) 5))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;2.37 不知到为什么我这里(map * v w)不能运行，提示说map只接受2个参数
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
(transpose (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

;2.38
;3/2
(accumulate / 1 (list 1 2 3))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;1/6
(fold-left / 1 (list 1 2 3))
;(1 (2 (3 ())))
(accumulate list nil (list 1 2 3))
;(((() 1) 2) 3)
(fold-left list nil (list 1 2 3))
;满足交换率

;2.39
(define (reverse sequence)
 (fold-left (lambda (x y) (cons y x)) nil sequence))
(define (reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))
(reverse (list 1 2 3 4))

;2.40
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-pairs
       (filter prime-sum?
               (unique-pairs n))))
(unique-pairs 10)

;2.41
(define (sum-s-pairs n s)
  (define (sum-s? pairs)
    (= s (accumulate + 0 pairs)))
  (filter sum-s?
          (accumulate append
                      nil
                      (accumulate append
                                  nil
                                  (map (lambda (i)
                                        (map (lambda (j)
                                              (map (lambda (k) (list i j k))
                                                   (enumerate-interval 1 j)))
                                             (enumerate-interval 1 i)))
                                       (enumerate-interval 1 n)))
         )))
(sum-s-pairs 5 10)


;; 2.42 http://cuitianyi.com/blog/sicp%E4%B9%A0%E9%A2%98%E8%A7%A3%E7%AD%94%EF%BC%9A%E7%AC%AC%E4%BA%8C%E7%AB%A0%EF%BC%88%E4%B8%8A%EF%BC%89/ 看那不懂
(define empty-board nil)
(define (safe? k p)
  (define (not-same p)
    (null? (filter (lambda (x) (= (car p) x)) (cdr p))))
  (and
   (not-same p)
   (not-same (map + p (enumerate-interval 1 k)))
   (not-same (map + p (reverse (enumerate-interval 1 k))))))
(define (adjoin-position n k r)(cons n r))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(queens 8)
