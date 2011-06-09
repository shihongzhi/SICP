;2.17
(define (last-pair items)
  (define (last-pair-iter a)
    (if (= 1 (length a))
        a
        (last-pair-iter (cdr a))))
  (last-pair-iter items))

(last-pair (list 23 72 149 34))

;2.18
(define nil '())
(define (reverse items)
  (define (rev a result)
    (if (null? a)
        result
        (rev (cdr a) (cons (car a) result))))
  (rev items nil))

(reverse (list 1 4 9 16 25))

;2.19
(define (no-more? a)
  (null? a))
(define (except-first-denomination a)
  (cdr a))
(define (first-denomination a)
  (car a))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else 
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values)))))

(define us-coins (list 50 10 25 5 1))

(cc 100 us-coins)
;coin-values的排序不会影响cc的答案

;2.20
(define (same-parity x . y)
  (define (iter type b)
    (if (null? b)
        nil
        (if (type (car b))
            (cons (car b) (iter type (cdr b)))
            (iter type (cdr b)))))
  (if (even? x)
      (cons x (iter even? y))
      (cons x (iter odd? y))))

(same-parity 1 2 3 4)

;2.21
(define (square x)
  (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) 
            (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))
(square-list-map (list 1 2 3 4))


;2.22
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        (cons answer nil)
        (iter (cdr things) (cons answer (square (car things))))))
  (iter (cdr items) (square (car items))))

(square-list-iter (list 2 3 4))
;北大的练习题：实现Louis的思想。不知到怎么做！（ps：上面的解法是错误的，结果和题里的第二个程序一样）
;ps:可以把第一个程序得到的结果reverse
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (reverse (iter items nil)))

(square-list (list 1 2 3 4 5))


;2.23
;cond一个判断中可以写两个语句，而if中就不行了
(define (for-each proc items)
  (cond ((null? items) #t)
        ((proc (car items)) (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;2.24 
;简单

;2.25
(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))

(define b (list (list 7)))
(car (car b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
;(1 2 3 4 5 6)
(append x y)
;((1 2 3) 4 5 6)
(cons x y)
;((1 2 3) (4 5 6))
(list x y)

;2.27  参考别人的，自己怎么这么笨啊！
(define (deep-reverse x)
  (if (not (pair? x))
      x
      (reverse (map deep-reverse x))))
(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)


;2.28
;(fringe (car (cdr x)))这里的car是为了防止nil的出现，即()
(define (fringe x)
  (if (not (pair? x))
      (list x)
      (append (fringe (car x)) (fringe (car (cdr x))))))

(fringe (list x x))

;2.29  题意不是很懂
(define (make-mobile left right)
  (list left right))
(define (make-branch lengths structure)
  (list lengths structure))
(define (left-branch x)
  (car x))
(define (right-branch x)
  (car (cdr x)))
(define (branch-length x)
  (car x))
(define (branch-structure x)
  (car (cdr x)))


;2.30
(define (square-tree x)
  (cond ((null? x) nil)
        ((not (pair? x)) (* x x))
        (else (cons (square-tree (car x))
                    (square-tree (cdr x))))))
(define (square-tree-map x)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree)))
       x))
(square-tree-map (list 1 
                   (list 2
                         (list 3 4)
                         5)
                   (list 6 7)))

;2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(tree-map square (list 5 
                   (list 2
                         (list 3 4)
                         5)
                   (list 6 7)))

;2.32 看到题目给的例子中前四个和后四个对称，才有了思路
(define (subsets s)
  (define (proc x)
    (append (list (car s)) x))
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map proc rest)))))

(subsets (list 1 2 3))

;针对序列的操作，价值在于能够模块化的操作

