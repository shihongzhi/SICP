(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (make-segment s e) (cons s e))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (midpoint-segment l)
  (make-point (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
              (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
;(1,1)
(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))

;2.3
(define (make-rect segment h)
  (cons segment h))
(define (seg-rect x) (car x))
(define (high-rect x) (cdr x))
;周长
(define (square x)
  (* x x))
(define (length-segment s)
  (sqrt (+ (square (abs (- (x-point (start-segment s)) (x-point (end-segment s))))) (square (abs (- (y-point (start-segment s)) (y-point (end-segment s))))))))
(define (c-rect r)
  (* 2 (+ (high-rect r) (length-segment (seg-rect r)))))
;面积
(define (s-rect r)
  (* (length-segment (seg-rect r)) (high-rect r)))

(s-rect (make-rect (make-segment (make-point 0 0) (make-point 2 2)) 2))
