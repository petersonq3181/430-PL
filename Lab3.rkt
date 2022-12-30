#lang typed/racket
(require typed/rackunit)

;; todo:
; - get parse002 test case to work
; - get ohno test case to work 


;; takes s-expression and returns true if matches a list
;; with a number, symbol 'chris, and a symbol
(define (parse000 [s : Sexp]) : Boolean
  (match s
    [(list (? number? a) 'chris (? symbol? b)) #t]
    [other #f]))
(check-equal? (parse000 (list 13 'chris 'symbollll)) #t)
(check-equal? (parse000 (list 'c)) #f)
(check-equal? (parse000 '()) #f)


;; takes s-expression and returns the pattern if matches a list
;; with a number, symbol 'chris, and a symbol, false otherwise 
(define (parse001 [s : Sexp]) : (U Boolean Symbol) 
  (match s
    [(list (? number? a) 'chris (? symbol? b)) b]
    [other #f]))
(check-equal? (parse001 (list 13 'chris 'symbollll)) 'symbollll)
(check-equal? (parse001 (list 'c)) #f)
(check-equal? (parse001 '()) #f)


;; takes s-expression and returns true if matches a list
;; with 3 elements, where the 2nd is a real number
(define (parse002 [s : Sexp]) : (U Boolean (Listof Real)) 
  (match s
    [(list a (list (? real? items) ...) c) (cast items (Listof Real))]
    [other #f]))
(check-equal? (parse002 '()) #f)
(check-equal? (parse002 (list 'c)) #f)
(check-equal? (parse002 (list 13 'chris 'symbollll)) #f)
(check-equal? (parse002 (list 13 (list 1) 'symbollll)) (list 1))


;; returns 'okay if the input is a number, and error otherwise
(define (ohno (x : Any)) : Symbol
  (match x
    [(? number? a) 'okay]
    [other (error 'ohno "expected Number, got ~e" x)]))

(check-equal? (ohno 23) 'okay)
(check-exn (regexp (regexp-quote "expected Number, got 'a"))
           (lambda () (ohno 'a)))





;; — define data type 
(define-type ArithC (U NumC PlusC MultC))
(struct NumC ([n : Real]))
(struct PlusC ([l : ArithC] [r : ArithC]))
(struct MultC ([l : ArithC] [r : ArithC]))
;; — define interpreter 
(define (interp [a : ArithC]) : Real
	(match a
          [(NumC n) n]
          [(PlusC l r) (+ (interp l) (interp r))]
          [(MultC l r) (* (interp l) (interp r))]))

(check-equal? (interp (NumC 4)) 4)
(check-equal? (interp (PlusC (NumC 3) (NumC 5))) 8)
(check-equal? (interp (MultC (NumC 3) (NumC 7))) 21)
(check-equal? (interp (PlusC (NumC 4) (MultC (NumC 5) (NumC 10)))) 54)

;; accepts an ArithC and returns the number of additions it contains
(define (num-adds [a : ArithC]) : Natural
  (match a
    [(NumC n) 0]
    [(PlusC l r) (+ 1 (+ (num-adds l) (num-adds r)))]
    [(MultC l r) (+ (num-adds l) (num-adds r))]))
(check-equal? (num-adds (NumC 4)) 0)
(check-equal? (num-adds (MultC (NumC 4) (NumC 3))) 0)
(check-equal? (num-adds (PlusC (NumC 4) (NumC 3))) 1)
(check-equal? (num-adds (MultC (PlusC (PlusC (NumC 0) (NumC 45)) (PlusC (NumC 4) (NumC 32))) (MultC (NumC 3) (PlusC (NumC 3) (NumC 4))))) 4)
