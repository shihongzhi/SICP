(define (f g)
  (g 2))

(f (lambda (z) (* z (+ z 1))))
;替换成(2 2),而前一个2不能代表为过程procedure，因此无法执行
(f f)
