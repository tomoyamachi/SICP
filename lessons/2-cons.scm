#lang scheme

(define (cons-x x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car-x z) (z 0))
(define (cdr-x z) (z 1))
(define (error-x z) (z 2))

(define x (cons-x 19 39))
(car-x x)
(cdr-x x)
(error-x x)
