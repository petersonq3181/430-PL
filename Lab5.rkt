#lang racket 
(require rackunit)

;; 1. warmup
(define one (lambda (f x) (f x)))

(define test-func1 (lambda (x) (+ x 3)))
(define test-func2 (lambda (x) (* x 2)))
(check-equal? (one test-func1 5) 8)
(check-equal? (one test-func2 5) 10)

;; ----- 2 

;; 1. takes func and arg and applies it to itself
(define two (lambda (f x) (f (f x))))
(check-equal? (two test-func1 5) 11)
(check-equal? (two test-func2 5) 20)

;; 2. takes func an arg and returns the arg
(define zero (lambda (f x) x))
(check-equal? (zero test-func1 3) 3)
(check-equal? (zero test-func2 5) 5)

;; 3. takes a number like function returns
;; new anon function which calls the input twice
;(define add1 (lambda (f) (lambda (a) (f (f a)))))
(define add1 (lambda (n) (lambda (f a) (n (f a)))))
(check-equal? (((add1 two) (λ (x) (* x 2))) 5) 40)

;; 4.
;(define add (lambda (f1 f2) (lambda (a b) )))
