#lang racket

(define (square x) (* x x))
(define (twoLargerNum a b c)
  (cond (
         (and (> b a) (> c a))
         (+ (square b) (square c))
         )
        (
         (and (> a b) (> c b))
         (+ (square a) (square c))
         )
        (
         (and (> a c) (> b c))
         (+ (square a) (square b))
         )
        )
  )

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)
  )

;; q1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y)
  )

;; q1.6
(define (cond-if p c a)
  (cond (p c)
        (else a))
  )

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
      )
  )

(define (improve guess x)
  (average guess (/ x guess))
  )

(define (average x y)
  (/ (+ x y) 2)
  )

(define (good-enough? guess x)
  (<
   (abs (- (square guess) x))
   1.0
   )
  )

(define (sqrt x)
  (sqrt-iter 1000.0 x)
  )

;; c1.1.7

(define (lexical-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0)
    )

;; c1.2.1

;; recursive(線形再帰)
(define (functrial n)
  (if (= n 1)
      1
      (* n (functrial (- n 1)))
      )
  )

;; 反復

(define (refunctrial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1)
              )
        )
    )
  (iter 1 1)
  )

;; q1.9

;;
;; (define (+ a b)
;;   (if (= a 0) b (inc (+ dec a) b))
;;   )

;; (define (+ a b)
;;   (if (= a 0) b (+ (dec a) (inc b)))
;;   )

;; q1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))
   )
  )

;; q1.11
;;Exercise 1.11. A function ff is defined by the rule that f(n)=nf(n)=n if n<3n<3 and f(n)=f(n−1)+2f(n−2)+3f(n−3)f(n)=f(n−1)+2f(n−2)+3f(n−3) if n>3n>3. Write a procedure that computes ff by means of a recursive process.

(define (f n)
  (A 0 n)
  )
(define (g n)
  (A 1 n)
  )
(define (h n)
  (A 2 n)
  )


;; q1.22

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

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      )
  )

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  )


(define (search-for-primes first last)
  (define (search-iter cur last)
    (if (<= cur last) (timed-prime-test cur))
    (if (<= cur last) (search-iter (+ cur 2) last)))
  (search-iter (if (even? first) (+ first 1) first)
               (if (even? last) (- last 1) last)))
