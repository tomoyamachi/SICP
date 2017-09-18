#lang scheme
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))



(define (timed-prime-test n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

;; (define (search-for-primes from n)
;;   (cond ((< n 0) (newline) 'done)
;;         ((even? from) (search-for-primes (+ from 1) (- n 1)))
;;         (else (timed-prime-test from)
;;               (search-for-primes (+ from 2) (- n 2)))))

(define (search-for-primes first last)
  (define (search-iter current last)
    (when (<= current last)
      (timed-prime-test current)
      (search-iter (+ current 2) last)))
  (define (odd-first)
    (if (even? first) (+ first 1) first))
  (search-iter (odd-first) last))


(search-for-primes 100000000000 100000000057)   ; 1e11
(search-for-primes 1000000000000 1000000000063) ; 1e12
(search-for-primes 10000000000000 10000000000103) ; 1e13
(search-for-primes 100000000000000 100000000000097) ; 1e14
(newline)
