#lang typed/racket
(require typed/rackunit)


; We believe we have completed the interface laid out for Assgn4


; Define type ExprC

(define-type ExprC (U NumC BinopC AppC IdC leq0?))
(struct NumC ([n : Real]) #:transparent)
(struct BinopC ([o : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct AppC ([fun : Symbol] [args : (U ExprC (Listof ExprC))]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

; Define structure FunDefC

(struct FunDefC ([name : Symbol] [arg : (U Symbol (Listof Symbol))]
                                 [body : ExprC]) #:transparent)


; Top level [ program --> parse --> AST --> interp --> answers ] function 
; This function accepts a Sexp and returns a Real

;Quin Logic


(define (top-interp [fun-sexps : Sexp]) : Real
  (define fun-list (parse-prog fun-sexps))
  (cond
    [(repeat-func-names? fun-list) (error 'top-interp "JYSS4: Cannot have multiple functions with the same name")]
    [else (interp-fns fun-list)]))

; Giselle Logic
; This works without breaking code for far

; (define (top-interp [fun-sexps : Sexp]) : Real
;   (interp-fns (parse-prog fun-sexps)))


; This function processes our Expressions AST and returns a number
; or an error (unreachable or dovide by zero)
; Parameters: ExprC, Listof FunDefC 
; Return: Real


; ###
(define (interp [e : ExprC] [funs : (Listof FunDefC)]) : Real
  (match e
    [(NumC n) n]
    
    [(IdC n) (cond
               [(symbol=? n 'init) 0]
               [else (error 'interp "JYSS4: Undefined/Unreachable")])]

    
    ; (note: order of these cases matters)
    ; 0 arg case
    [(AppC f '())
     (define fd (get-fundef f funs))
     (define args (cast (FunDefC-arg fd) (Listof Symbol)))
     (interp (FunDefC-body fd) funs)]

    
    ; >1 args case (note: still need to work this out) 
    [(AppC f (list arguments ...)) (define fd (get-fundef f funs))
                                          (define args (cast arguments (Listof ExprC)))
                                          (define arg-val-list
                                            (map
                                             (lambda (i) (define e (cast i ExprC)) (NumC (interp e funs))) args))
                                          (define forr (cast (FunDefC-arg fd) (Listof Symbol)))
                                          (interp (subst-many arg-val-list forr (FunDefC-body fd)) funs)]
    
    ; 1 arg case 
    [(AppC f a) (define fd (get-fundef f funs))
                (define forr (cast (FunDefC-arg fd) Symbol))
                (interp
                 (subst (NumC (interp (cast a ExprC) funs))
                        forr
                        (FunDefC-body fd))
                 funs)]
    
    [(BinopC '/ l r) ((binop '/) (interp l funs)
                                 (if (= (interp r funs) 0)
                                     (error 'interp "JYSS4: Divide by Zero Error")
                                     (interp r funs)))]
    [(BinopC o l r) ((binop o) (interp l funs) (interp r funs))]
    [(leq0? test then else) (if (<= (interp test funs) 0) (interp then funs) (interp else funs))]))

; Giselle Logic
; Doesn't work, but this is my thinking

; (define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
;   (match exp
;     [(NumC n) n]
;     [(IdC n) (error 'interp "JYSS4: Unreachable")]
;     [(AppC f a) (define fd (get-fundef f funs)) (interp (subst (for/list ([b a]) (NumC (interp b funs)))
;                                                                (FunDefC-arg fd)
;                                                                (FunDefC-body fd)) funs)]
;     [(BinopC '/ l r) ((binop '/) (interp l funs)
;                                  (if (= (interp r funs) 0)
;                                      (error 'interp "JYSS4: Divide by Zero")
;                                      (interp r funs)))]
;     [(BinopC o l r) ((binop o) (interp l funs) (interp r funs))]
;     [(leq0? test then else) (if (<= (interp test funs) 0) (interp then funs) (interp else funs))]
;     ))
; 


; This function is our interpreter for JYSS3 functions
; Parameters: Listof FunDefC 
; Return: Real

(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (cond
    [(empty? funs) (error 'interp-fns "JYSS4: Undefined")]
    [(cons? funs) (interp (FunDefC-body (get-fundef 'main funs)) funs)]))

; This function processes a symbol and returns matching operator

(define (binop [o : Symbol]) : (-> Real Real Real)
  (match o
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]))

; This function substitutes an Expression with the wanted Expression
; Parameters: ExprC, Symbol (what to replace with), ExprC
; Return: ExprC

(define (subst [what : ExprC] [for : Symbol] [in : (U ExprC (Listof ExprC))]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(BinopC o l r) (BinopC o (subst what for l) (subst what for r))]
    [(leq0? test then else) (leq0? (subst what for test) (subst what for then)
                                   (subst what for else))]))


;; returns the length of a list 

(define (length [l : (Listof Any)]) : Natural
  (cond
    [(empty? l)  0]
    [(cons? l) (+ 1 (length (rest l)))]))

;; substitutes multiple paramters' values into a givnen expression 

(define (subst-many [what : (Listof ExprC)] [forr : (Listof Symbol)] [in : (U ExprC (Listof ExprC))]) : ExprC
  (match forr
    ['() (cast in ExprC)]
    [other (cond
       [(and (equal? (length what) (length forr)) (> (length forr) 0))
        (define arg (first what))
        (define par (first forr))
        (match in
         [(NumC n) in]
         [(IdC s) (cond
               [(symbol=? s par) (subst-many (rest what) (rest forr) arg)]
               [else (subst-many (rest what) (rest forr) in)])]
         [(AppC f a) (subst-many (rest what) (rest forr) (AppC f (subst arg par a)))]
         [(BinopC o l r) (subst-many (rest what) (rest forr) (BinopC o (subst arg par l) (subst arg par r)))]
         [(leq0? test then else) (subst-many (rest what) (rest forr) (leq0? (subst arg par test) (subst arg par then)
                                   (subst arg par else)))])]
       [else (error 'subst-many "JYSS4: Incorrect number of args given")])]))


; Giselle Logic
; My thinking that follows the logic of interp

; (define (subst [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
;   (cond
;     [(= (length what) (length for))
;      (match in
;        [(NumC n) in]
;        [(IdC s) (cond
;                   [(list? (member s for)) (subst what for (list-ref what (cast (index-of for s) Integer)))]
;                   [else in])] 
;        [(AppC f a) (AppC f (for/list ([b a]) (subst what for b)))]
;        [(BinopC o l r) (BinopC o (subst what for l) (subst what for r))]
;        [(jeq0? test then else) (leq0? (subst what for test) (subst what for then) (subst what for else))]
;        )]
;     [else (error 'subst "JYSS4: Incorrect number of Arguments")]))


; This function grabs the correct function definition or raises an error
; Parameters: Symbol (name of function), Listof FunDefC
; Return: FunDefC

(define (get-fundef [s : Symbol] [funs : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? funs) (error 'get-fundef "JYSS4: Unimplemented Function")]
    [(cons? funs) (cond
                    [(equal? s (FunDefC-name (first funs))) (first funs)]
                    [else (get-fundef s (rest funs))])]))

; This function parses a expression
; Parameters: Sexp, Listof FunDefC
; Return: nothing!

(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))


; This function  This function parses a function definition
; Parameters: Sexp
; Return: FunDefC

(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    ; 1 arg case 
    [(list 'fn (list (? symbol? name) (? symbol? param)) body)
     (cond
       [(or (bad-name? name) (not (or (real? body) (list? body)))) (error 'parse-fundef
                                                  "JYSS4: Invalid Function Definition")]
       [else (FunDefC name param (parse body))])]
    ; 0 or >1 arg case (aka a list) 
    [(list 'fn (list (? symbol? name) (? symbol? params) ...) body)
     (define pars (cast params (Listof Symbol)))
     (cond
       [(or (or (bad-name? name) (symbol-repeats? pars)) (not (or (real? body)

   (list? body)))) (error 'parse-fundef "JYSS4: Invalid Function Definition")]
       [else (FunDefC name pars (parse body))])]
    [other (error 'parse-fundef "JYSS4: Invalid Function Definition")]))

#|
; Giselle Logic


(define (parse-fundef [s : Sexp]) : FunDefC
   (match s
     [(list 'def (list (? symbol? name) param ...) body)
      (FunDefC name (cast param (Listof Symbol)) ;list of symbols is a list of sexp
               (parse body))]
     [else (error 'parse-fundef "JYSS4: Invalid Function Definition")]))
|#


; checks if a list of symbols has any identical symbols
; returns true if it does, false otherwise 

(define (symbol-repeats? [pars : (Listof Symbol)]) : Boolean
  (match pars
    ['() #f]
    [(cons f '()) #f]
    [(cons f r) (cond
                  [(eq? f (first r)) #t]
                  [(symbol-repeats? (cons f (rest r))) #t]
                  [(symbol-repeats? r) #t]
                  [else #f])]))

; checks if list of FunDefC's has any which have matching names
; returns true if so, false otherwise 

(define (repeat-func-names? [fs : (Listof FunDefC)]) : Boolean
  (define fun-names-list (map (lambda(i) (FunDefC-name i)) fs))
  (symbol-repeats? fun-names-list))

; returns true if the given symbol is any of the following special patterns

(define (oper-symbol? [s : Symbol]) : Boolean
  (match s
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    [other #f]))

; returns true if the given symbol is any of the patterns

(define (bad-name? [s : Symbol]) : Boolean
  (cond
    [(oper-symbol? s) #t]
    [else (match s
            ['list #t]
            ['cons #t]
            ['match #t]
            ['cond #t]
            [other #f])]))

; helper function to check if a list contains a single weird symbol
; if it does, return true, otherwise return false

(define (contains-oper-symbol? [s : (Listof Symbol)]) : Boolean
  (match s
    ['() #f]
    [(cons f r) (or (oper-symbol? f) (contains-oper-symbol? r))]))


; This function parses an expression
; Parameters: Sexp
; Return: ExprC

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? s)
     #:when (boolean? (member s '(+ * - / leq0? fn)))
     (IdC s)]

    [(list 'leq0? test then else) (leq0? (parse test) (parse then) (parse else))]
    [(list 'leq0?) (error 'parse "JYSS4: Invalid Syntax")]

    ; AppC w/ 2 args must be carefully differentiated from BinopC
    [(list (? symbol? o) l r) (cond
                                [(oper-symbol? o) (BinopC o (parse l) (parse r))]
                                [else (AppC o (list (parse l) (parse r)))])]

    ; 0 arg case
    [(list (? symbol? fname)) (cond [(bad-name? fname) (error 'parse
                                                              "JYSS4: Invalid Syntax")]
                                    [else (AppC fname '())])]

    ; 1 arg case
    [(list (? symbol? fname) s) (cond [(bad-name? fname) (error 'parse
                                                                "JYSS4: Invalid Syntax")]
                                    [else (AppC fname (parse s))])]

    ; >1 arg case 
    [(list (? symbol? fname) s ...) (cond [(bad-name? fname) (error 'parse
                                                               "JYSS4: Invalid Syntax")]
                                    [else (define args (cast s (Listof Sexp)))
                                          (AppC fname (map parse args))])]
    
    [else (error 'parse "JYSS4: Invalid Syntax")]))


#|
; Giselle's Logic

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)] ;NumC case
    
    [(? symbol? s)
     #:when (boolean? (member s '(+ * - / leq0? def)))
     (IdC s)] ;IdC case
    
    [(list (? symbol? f) a ...)
     #:when (false? (member f '(+ * - / leq0? def)))
     (AppC f (for/list ([x a]) (parse x)))] ;AppC param case
    
    [(list (? symbol? o) l r)
     (BinopC o (parse l) (parse r))] ;BinopC case
    
    [(list 'leq0? test then else) (leq0? (parse test) (parse then) (parse else))] ;leq0? case

    [else (error 'parse "JYSS4: Invalid Syntax")]
    ))
|#





; ------- Test cases

; top-interp test cases

(check-equal? (top-interp '{{fn {f x} {+ 1 x}}
                            {fn {g y} {* 1 y}}
                            {fn {main init} {+ {f 2} {g 5}}}
                            }
                          )
              8)
(check-equal? (top-interp '{{fn {f x} {* 2 x}}
                            {fn {g y} {* 1 y}}
                            {fn {main init} {- {f 3} {g 3}}}
                            }
                          )
              3)
(check-equal? (top-interp '{{fn {f x} {+ x {* 2 {g x}}}}
                            {fn {g y} {* 7 y}}
                            {fn {ff x} {+ x x}}
                            {fn {main init} {+ {f 2} {g 3}}}
                            }
                          )
              51)
(check-equal? (top-interp '{{fn {decrement x} {leq0? x x {decrement {- x 1}}}}
                            {fn {chkr y} {leq0? {+ y 0.5} {+ y -1} {* -1 y}}}
                            {fn {round z} {+ z {chkr {decrement z}}}}
                            {fn {main init} {round 101.7}}
                            }
                          )
              102.0)
(check-equal? (top-interp '{{fn {decrement x} {leq0? x x {decrement {- x 1}}}}
                            {fn {chkr y} {leq0? {+ y 0.5} {+ y -1} {* -1 y}}}
                            {fn {round z} {+ z {chkr {decrement z}}}}
                            {fn {main init} {round 1.7}}
                            }
                          )
              2.0)
(check-equal? (top-interp '{{fn {decrement x} {leq0? x x {decrement {- x 1}}}}
                            {fn {chkr y} {leq0? {+ y 0.5} {* -1 {+ y 1}} {* -1 y}}}
                            {fn {round z} {+ z {chkr {decrement z}}}}
                            {fn {main init} {round 1.2}}
                            }
                          )
              1.0)
(check-equal? (top-interp '{{fn {minus-five x} {+ x {* -1 5}}}
                            {fn {main init} {minus-five {+ 8 1}}}}) 4)
(check-equal? (top-interp '{{fn {minus-five x} {+ x {* -1 5}}}
                            {fn {main init} {minus-five {+ 8 init}}}}) 3)
(check-equal? (top-interp '{{fn {main init} {leq0? {* 3 1} 3 {+ 2 9}}}}) 11)
(check-exn (regexp (regexp-quote "interp: JYSS4: Divide by Zero Error"))
           (lambda () (top-interp '{{fn {main init} {/ 1 {+ 0 0}}}})))
(check-exn (regexp (regexp-quote "interp: JYSS4: Divide by Zero Error"))
           (lambda () (top-interp '((fn (ignoreit x) (+ 2 2)) (fn (main init)
 (ignoreit (/ 1 (+ 0 0))))))))
(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (top-interp '{{fn {f x} {}}
                                    {fn {main init} {+ 22 34}}})))
(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (top-interp '{{fn {f x} {}}
                                    {fn {main} {+ 0 4}}})))

(check-exn (regexp (regexp-quote "top-interp: JYSS4: Cannot have multiple functions with the same name"))
           (lambda () (top-interp '{{fn {f x} {+ x 1}}
                                    {fn {f x y} {+ x y}}
                                    {fn {main init} {+ 0 4}}})))

; ### 

; interp test cases

(define test-function (list (FunDefC 'f 'x (NumC 1))))
(check-equal? (interp (BinopC '+ (NumC 1) (NumC 2)) test-function) 3)
(check-equal? (interp (BinopC '+ (NumC 5) (NumC 5)) test-function) 10)
(check-equal? (interp (BinopC '+ (NumC 100) (NumC 2)) test-function) 102)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (NumC 3)) test-function) 9)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 5) (NumC 5)) (NumC 3)) test-function) 30)
(check-equal? (interp (BinopC '- (BinopC '+ (NumC 3) (NumC 3)) (NumC 3)) test-function) 3)
(check-equal? (interp (BinopC '- (NumC 4) (NumC 1)) test-function) 3)
(check-equal? (interp (BinopC '/ (NumC 4) (NumC 2)) test-function) 2)
(check-equal? (interp (BinopC '/ (NumC 16) (NumC 2)) test-function) 8)
(check-equal? (interp (leq0? (NumC 1) (NumC 2) (NumC 3)) test-function) 3)
(check-equal? (interp (leq0? (NumC 0) (NumC 2) (NumC 3)) test-function) 2)
(check-equal? (interp (leq0? (NumC -10) (BinopC '/ (NumC 4) (NumC 2)) (BinopC
 '- (NumC 4) (NumC 1))) test-function) 2)
(check-exn (regexp (regexp-quote "interp: JYSS4: Divide by Zero Error"))
           (lambda () (interp (BinopC '/ (NumC 800) (NumC 0)) test-function)))
(check-exn (regexp (regexp-quote "interp: JYSS4: Undefined/Unreachable"))
           (lambda () (interp (IdC 'a) test-function)))
(check-exn (regexp (regexp-quote "interp: JYSS4: Divide by Zero Error"))
           (lambda () (interp (AppC 'main (NumC 0))
                              (list
                               (FunDefC 'ignoreit 'x
                                        (BinopC '+ (NumC 3) (NumC 4)))
                               (FunDefC 'main 'init (AppC 'ignoreit
                                                          (BinopC '/ (NumC 1)
(BinopC '+ (NumC 0) (NumC 0)))))))))

(define test-func-list (list
                        (FunDefC 'f 'x (NumC 1))
                        (FunDefC 'fa '() (BinopC '+ (NumC 1) (NumC 2)))
                        (FunDefC 'fb (list 'a 'b) (BinopC '+ (IdC 'a) (NumC 1)))
                        (FunDefC 'fc (list 'a 'b) (BinopC '+ (IdC 'a) (IdC 'b)))
                        (FunDefC 'fd (list 'a 'b) (BinopC '+ (IdC 'a) (IdC 'b)))
                        (FunDefC 'fe (list 'a 'b 'par) (BinopC '+ (IdC 'a) (AppC 'f (BinopC '+ (IdC 'b) (IdC 'par)))))
                        (FunDefC 'ff (list 'x 'y) (AppC 'fc (list (IdC 'x) (IdC 'y))))))
(check-equal? (interp (AppC 'fa '()) test-func-list) 3)
(check-equal? (interp (AppC 'f (NumC 1)) test-func-list) 1)
(check-equal? (interp (AppC 'fc (list (NumC 34) (NumC 3))) test-func-list) 37)




; interp-fns test cases

(check-exn (regexp (regexp-quote "interp-fns: JYSS4: Undefined"))
           (lambda () (interp-fns'())))
(check-equal? (interp-fns
               (list
                (FunDefC 'f 'x (BinopC '+ (NumC 1) (IdC 'x)))
                (FunDefC 'g 'y (BinopC '* (NumC 1) (IdC 'y)))
                (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                             (AppC 'g (NumC 3)))))) 6)
(check-equal? (interp-fns
               (list
                (FunDefC 'f 'x (BinopC '+ (NumC 1) (IdC 'x)))
                (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                             (AppC 'f (NumC 3)))))) 7)
(check-equal? (interp-fns
               (list
                (FunDefC 'f 'x (BinopC '+ (AppC 'g (IdC 'x)) (IdC 'x)))
                (FunDefC 'g 'y (BinopC '* (NumC 3) (IdC 'y)))
                (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                             (AppC 'g (NumC 3)))))) 17)
(check-equal? (interp-fns
               (list
                (FunDefC 'f 'x (BinopC '+ (AppC 'g (IdC 'x)) (IdC 'x)))
                (FunDefC 'g 'y (BinopC '* (NumC 3) (IdC 'y)))
                (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 4))
                                             (AppC 'g (NumC 7)))))) 37)

; parse-prog test cases

(check-equal? (parse-prog '{{fn {f x} {+ 1 x}}
                            {fn {g y} {* 1 y}}
                            {fn {main init} {+ {f 2} {g 3}}}
                            })
              (list
               (FunDefC 'f 'x (BinopC '+ (NumC 1) (IdC 'x)))
               (FunDefC 'g 'y (BinopC '* (NumC 1) (IdC 'y)))
               (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                            (AppC 'g (NumC 3))))))
(check-equal? (parse-prog '{{fn {f x} {+ x {* 2 {g x}}}}
                            {fn {g y} {* 1 y}}
                            {fn {main init} {+ {f 2} {g 3}}}})
              (list
               (FunDefC 'f 'x (BinopC '+ (IdC 'x) (BinopC '* (NumC 2) (AppC 'g (IdC 'x)))))
               (FunDefC 'g 'y (BinopC '* (NumC 1) (IdC 'y)))
               (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                            (AppC 'g (NumC 3))))))
(check-equal? (parse-prog '((fn (ignoreit x) (+ 3 4)) (fn (main init) (ignoreit (/ 1 (+ 0 0))))))
              (list (FunDefC 'ignoreit 'x (BinopC '+ (NumC 3) (NumC 4)))
                    (FunDefC 'main 'init (AppC 'ignoreit (BinopC '/ (NumC 1)
 (BinopC '+ (NumC 0) (NumC 0)))))))


; subst-many test cases

(check-equal? (subst-many (list (NumC 1) (NumC 2)) (list 'a 'b) (NumC 3))
              (NumC 3))
(check-equal? (subst-many (list (NumC 1) (NumC 2)) (list 'a 'b) (IdC 'a))
              (NumC 1))
(check-equal? (subst-many (list (NumC 1) (NumC 2)) (list 'a 'b) (IdC 'c))
              (IdC 'c))
(check-equal? (subst-many (list (NumC 1)) (list 'x) (AppC 'f (IdC 'x)))
              (AppC 'f (NumC 1)))
(check-equal? (subst-many (list (NumC 1)) (list 'x) (leq0? (NumC 1) (NumC 2) (NumC 3)))
              (leq0? (NumC 1) (NumC 2) (NumC 3)))
(check-equal? (subst-many (list (NumC 1) (NumC 2)) (list 'a 'b) (BinopC '+ (IdC 'a) (IdC 'b)))
              (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (subst-many (list (NumC 1) (NumC 2)) (list 'a 'b) (BinopC '+ (IdC 'b) (BinopC '- (NumC 2) (NumC 1))))
              (BinopC '+ (NumC 2) (BinopC '- (NumC 2) (NumC 1))))
(check-equal? (subst-many (list (NumC 20) (NumC 30)) (list 'a 'b) (BinopC '+ (IdC 'b) (BinopC '- (IdC 'a) (IdC 'a))))
              (BinopC '+ (NumC 30) (BinopC '- (NumC 20) (NumC 20))))
(check-equal? (subst-many (list (NumC 1) (NumC 2)) (list 'a 'b) (BinopC '+ (NumC 10) (NumC 11)))
              (BinopC '+ (NumC 10) (NumC 11)))
(check-exn (regexp (regexp-quote "subst-many: JYSS4: Incorrect number of args given"))
           (lambda () (subst-many (list (NumC 1) (NumC 2)) (list 'a) (BinopC '+ (IdC 'a) (IdC 'b)))))
(check-exn (regexp (regexp-quote "subst-many: JYSS4: Incorrect number of args given"))
           (lambda () (subst-many (list (NumC 1)) (list 'a 'b 'c) (BinopC '+ (IdC 'a) (IdC 'b)))))
(check-exn (regexp (regexp-quote "subst-many: JYSS4: Incorrect number of args given"))
           (lambda () (subst-many '() (list 'a 'b) (BinopC '+ (IdC 'a) (IdC 'b)))))

; parse-fundef test cases
(check-equal? (parse-fundef '{fn {f x} {+ 2 1}}) (FunDefC 'f 'x (BinopC '+ (NumC 2) (NumC 1))))
(check-equal? (parse-fundef '{fn {g y} {* 1 y}}) (FunDefC 'g 'y (BinopC '* (NumC 1) (IdC 'y))))
(check-equal? (parse-fundef '{fn {h z} 6}) (FunDefC 'h 'z (NumC 6)))
(check-equal? (parse-fundef '{fn {f x} {+ 2 1}})
              (FunDefC 'f 'x (BinopC '+ (NumC 2) (NumC 1))))
(check-equal? (parse-fundef '{fn {func a} {- a 4}})
              (FunDefC 'func 'a (BinopC '- (IdC 'a) (NumC 4))))
(check-equal? (parse-fundef '{fn {my-func my-par} {+ 1 {/ my-par my-par}}})
              (FunDefC 'my-func 'my-par (BinopC '+ (NumC 1) (BinopC '/ (IdC 'my-par) (IdC 'my-par)))))
(check-equal? (parse-fundef '{fn {f x} {+ x {g x}}})
              (FunDefC 'f 'x (BinopC '+ (IdC 'x) (AppC 'g (IdC 'x)))))
(check-equal? (parse-fundef '{fn {f} {+ 1 2}})
              (FunDefC 'f '() (BinopC '+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef '{fn {f a b} {+ 1 2}})
              (FunDefC 'f (list 'a 'b) (BinopC '+ (NumC 1) (NumC 2))))
(check-equal? (parse-fundef '{fn {f a b} {+ a b}})
              (FunDefC 'f (list 'a 'b) (BinopC '+ (IdC 'a) (IdC 'b))))
(check-equal? (parse-fundef '{fn {my-func a b third-par} {+ 1 {g a b (- 0 third-par)}}})
              (FunDefC 'my-func (list 'a 'b 'third-par) (BinopC '+ (NumC 1) (AppC 'g
                         (list (IdC 'a) (IdC 'b) (BinopC '- (NumC 0) (IdC 'third-par)))))))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {+ x} {} 6})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {+ x} 6})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {+ x} {+ x 10}})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {cond x y} {+ x 10}})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {list x y} {+ x 10}})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {f x y} {+ x 10} {+ x 1}})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {f x y}})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {f x y x} {+ 1 {- x 0}}})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS4: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {f abcd abcd x y z} {+ 1 {- x abcd}}})))

; symbol-repeats? test cases

(check-equal? (symbol-repeats? '()) #f)
(check-equal? (symbol-repeats? (list 'a)) #f)
(check-equal? (symbol-repeats? (list 'a 'b 'c 'd 'asdf)) #f)
(check-equal? (symbol-repeats? (list 'a 'a 'c 'a 'asdf)) #t)
(check-equal? (symbol-repeats? (list 'test 'test)) #t)
(check-equal? (symbol-repeats? (list 'x 'y 'y 'x)) #t)
(check-equal? (symbol-repeats? (list 'x 'y 'a 'x)) #t)
(check-equal? (symbol-repeats? (list 'a 'b 'c 'c)) #t)

; repeat-func-names? test cases

(check-equal? (repeat-func-names? (list (FunDefC 'f (list 'x) (NumC 1)))) #f)
(check-equal? (repeat-func-names? (list (FunDefC 'f (list 'x) (NumC 1))
                                        (FunDefC 'func '() (BinopC '+ (NumC 1) (NumC 2))))) #f)
(check-equal? (repeat-func-names? (list (FunDefC 'f (list 'x 'a)
                   (NumC 1)) (FunDefC 'main '() (NumC 1)) (FunDefC 'f '() (NumC 1)))) #t)

; oper-symbol? test cases

(check-equal? (oper-symbol? 's) #f)
(check-equal? (oper-symbol? '+) #t)
(check-equal? (oper-symbol? '-) #t)
(check-equal? (oper-symbol? '*) #t)
(check-equal? (oper-symbol? '/) #t)

; bad-name? test cases

(check-equal? (bad-name? '-) #t)
(check-equal? (bad-name? 'cons) #t)
(check-equal? (bad-name? 'list) #t)
(check-equal? (bad-name? 'match) #t)
(check-equal? (bad-name? 'cond) #t)
(check-equal? (bad-name? 'f) #f)

; contains-oper-symbol? test cases

(check-equal? (contains-oper-symbol? (list 'f 'a 'b 'c)) #f)
(check-equal? (contains-oper-symbol? (list 'f 'a '+ 'c)) #t)
(check-equal? (contains-oper-symbol? (list '*)) #t)
(check-equal? (contains-oper-symbol? (list '* '- 'c '-)) #t)
(check-equal? (contains-oper-symbol? '()) #f)
(check-equal? (contains-oper-symbol? (list 'f)) #f)

; length test cases

(check-equal? (length '()) 0)
(check-equal? (length '(1 2 3)) 3)
(check-equal? (length (list 'a 'b 'cde 'f)) 4)

; parse test cases

(check-equal? (parse '168) (NumC 168))
(check-equal? (parse (list '+ (list '- 0 1) 2)) (BinopC '+ (BinopC '-
                                             (NumC 0) (NumC 1)) (NumC 2)))

(check-equal? (parse '{+ 2 2}) (BinopC '+ (NumC 2) (NumC 2)))
(check-equal? (parse '{* {+ 1 2} 3}) (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (NumC 3)))

(check-equal? (parse '{- 16 1}) (BinopC '- (NumC 16) (NumC 1)))
(check-equal? (parse '{/ 16 2}) (BinopC '/ (NumC 16) (NumC 2)))

(check-equal? (parse '{leq0? 1 2 3}) (leq0? (NumC 1) (NumC 2) (NumC 3)))

(check-equal? (parse (list '+ 1 2)) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse (list 'leq0? 'x 1 2)) (leq0? (IdC 'x) (NumC 1) (NumC 2)))
(check-equal? (parse '{f 0 1}) (AppC 'f (list (NumC 0) (NumC 1))))
(check-equal? (parse '{f 0 1 2 3 4}) (AppC 'f (list (NumC 0) (NumC 1)
                                         (NumC 2) (NumC 3) (NumC 4))))
(check-equal? (parse '{f}) (AppC 'f '()))

(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (parse '{list})))

(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (parse '{list x})))

(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (parse '{list {list 1 2 3} 4 5 6})))

(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (parse '{+ / 123})))

(check-exn (regexp (regexp-quote "parse: JYSS4: Invalid Syntax"))
           (lambda () (parse '{+ / 2})))

(check-exn (regexp (regexp-quote "JYSS4: Invalid Syntax"))
           (lambda () (parse '{leq0?})))

; subst test cases

(check-equal? (subst (IdC 'a) 'b (IdC 'a)) (IdC 'a))
(check-equal? (subst (NumC 0) 'init (AppC 'ignoreit (BinopC '/ (NumC 1)
(BinopC '+ (NumC 0) (NumC 0)))))
              (AppC 'ignoreit (BinopC '/ (NumC 1) (BinopC '+ (NumC 0) (NumC 0)))))
(check-equal? (subst (BinopC '/ (NumC 1) (BinopC '+ (NumC 0) (NumC 0))) 'x
                     (BinopC '+ (NumC 3) (NumC 4))) (BinopC '+ (NumC 3) (NumC 4)))
(check-equal? (subst (BinopC '+ (NumC 1) (NumC 2)) 'x
                     (AppC 'f (IdC 'x)))
              (AppC 'f (BinopC '+ (NumC 1) (NumC 2))))
(check-equal? (subst (NumC 1) 'x
                     (BinopC '+ (IdC 'x) (NumC 2)))
              (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (subst (BinopC '* (NumC 1) (NumC 2)) 'x
                     (BinopC '+ (IdC 'x) (NumC 2)))
              (BinopC '+ (BinopC '* (NumC 1) (NumC 2)) (NumC 2)))

; get-fundef test cases

(check-exn (regexp (regexp-quote "get-fundef: JYSS4: Unimplemented Function"))
           (lambda () (get-fundef 'a '())))
