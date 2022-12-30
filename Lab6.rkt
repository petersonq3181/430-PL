#lang typed/racket
(require typed/rackunit)


;; ExprC
;; the base exprs are: NumC StrC IdC (don't contain self-references) 
(define-type ExprC (U NumC StrC AppC IdC IfC LamC))
(struct NumC ([n : Real])
  #:transparent)
(struct StrC ([s : String])
  #:transparent)
(struct IdC ([name : Symbol])
  #:transparent)
(struct AppC ([fun : ExprC] [arg : (Listof ExprC)])
  #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC])
  #:transparent)
(struct LamC ([params : (Listof Symbol)] [body : ExprC])
  #:transparent)


;; randomly returns one of the symbols in the fixed set 
(define (random-symbol) : Symbol
  (random-symbol-helper (list 'a 'b 'c 'd '+ '- '* '/) (random 0 8)))
(define (random-symbol-helper [l : (Listof Symbol)] [i : Natural]) : Symbol
  (if (= i 0)
      (first l)
      (random-symbol-helper (rest l) (- i 1))))


;; creates a random base expression
(define (random-base-term) : ExprC
  (define base-num (random 0 3))
  (cond
    [(= base-num 0) (IdC (random-symbol))]
    [(= base-num 1) (NumC (random 0 4294967087))]
    [else
     (define ALPHABET "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY ")
     (define str-len (random 0 32))
     (define rand-str-lst (build-list str-len (lambda (i) (select-random-item ALPHABET))))
     (StrC (list->string rand-str-lst))]))
(define (select-random-item [seq : String])
  (sequence-ref seq (random (sequence-length seq))))


;; creates random ExprC tree of a maximum specified depth
(define (random-term [d : Real]) : ExprC
  (cond
    [(<= d 1) (random-base-term)]
    [else
     (define rand-num (random 0 3))
     (cond
       [(= rand-num 0) (IfC (random-term (- d 1)) (random-term (- d 1)) (random-term (- d 1)))]
       [(= rand-num 1)
        (define arg-list (build-list (random 0 3) (lambda (i) (random-term (- d 1)))))
        (AppC (random-term (- d 1)) arg-list)]
       [else
        (define par-list (build-list (random 0 3) (lambda (i) (random-symbol))))
        (LamC par-list (random-term (- d 1)))])]))


;; unparse an ExprC back into a s-expression
(define (unparse [parsed_exp : ExprC]) : Sexp
  (match parsed_exp
    [(NumC n) n]
    [(IdC x) x]
    [(StrC str) str]
    [(LamC args body) (list 'proc args `go (unparse body))]
    [(AppC fun args) (define arg (for/list : (Listof Sexp) ([i (cast args (Listof ExprC))]) (unparse (cast i ExprC)))) (cons (unparse fun) arg)]
    [(IfC if then else) (list 'if (unparse if) (unparse then) (unparse else))])
  )
(check-equal? (unparse (NumC 1)) 1)
(check-equal? (unparse (IfC (NumC 1) (IdC 'a) (IdC 'b))) '(if 1 a b))

(define (quiz) : ExprC
  (define rand-term (random-term (random 0 8)))
  (print (unparse rand-term))
  rand-term)


(check-equal? (unparse (LamC (list 'x) (IdC 'x))) '(proc (x) go x))

(define secret (quiz))
