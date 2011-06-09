;2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-aplit painter (- n 1))))
        (below painter (below smaller smaller)))))

;2.45
(define (split a b)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split a b) painter (- n 1))))
          (a painter (b smaller smaller))))))


