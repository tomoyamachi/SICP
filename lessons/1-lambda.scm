#lang scheme

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (pi-sum-lambda a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a
       (lambda (x) (+ x 4)) b

       ))

(* 8 (pi-sum-lambda 1 1000))
