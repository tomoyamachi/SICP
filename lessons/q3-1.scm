#lang scheme

(define (make-accumulator num)
  (display num)
  (newline)
  (lambda (additional)
    (begin (set! num (+ num additional)) num))
    )


(define A (make-accumulator 5))
(A 10)
(A 10)
