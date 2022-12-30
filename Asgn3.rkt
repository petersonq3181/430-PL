#lang typed/racket
(require typed/rackunit)


; We believe we have completed the inteface laid out for Assgn3


; Define type ExprC
(define-type ExprC (U NumC BinopC AppC IdC leq0?))
(struct NumC ([n : Real]) #:transparent)
(struct BinopC ([o : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

; Define structure FunDefC
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)


; Top level [ program --> parse --> AST --> interp --> answers ] function 
; This function accepts a Sexp and returns a Real
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))

; This function is our interpreter for JYSS3 functions
; Parameters: Listof FunDefC 
; Return: Real
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (cond
    [(empty? funs) (error 'interp-fns "JYSS3: Undefined")]
    [(cons? funs) (interp (FunDefC-body (get-fundef 'main funs)) funs)]))

; This function processes our Expressions AST and returns a number or an error (unreachable or dovide by zero)
; Parameters: ExprC, Listof FunDefC 
; Return: Real
(define (interp [e : ExprC] [funs : (Listof FunDefC)]) : Real
  (match e
    [(NumC n) n]
    [(IdC n) (cond
               [(symbol=? n 'init) 0]
               [else (error 'interp "JYSS3: Undefined/Unreachable")])]
    [(AppC f a) (define fd (get-fundef f funs)) (interp (subst (NumC (interp a funs))
                                                               (FunDefC-arg fd)
                                                               (FunDefC-body fd)) funs)]
    [(BinopC '/ l r) ((binop '/) (interp l funs)
                                 (if (= (interp r funs) 0)
                                     (error 'interp "JYSS3: Divide by Zero Error")
                                     (interp r funs)))]
    [(BinopC o l r) ((binop o) (interp l funs) (interp r funs))]
    [(leq0? test then else) (if (<= (interp test funs) 0) (interp then funs) (interp else funs))]))


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
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(BinopC o l r) (BinopC o (subst what for l) (subst what for r))]
    [(leq0? test then else) (leq0? (subst what for test) (subst what for then) (subst what for else))]))

; This function grabs the correct function definition or raises an error
; Parameters: Symbol (name of function), Listof FunDefC
; Return: FunDefC
(define (get-fundef [s : Symbol] [funs : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? funs) (error 'get-fundef "JYSS3: Unimplemented Function")]
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
    [(list 'fn (list (? symbol? name) (? symbol? param)) (? real? body))
     (cond
       [(or (oper-symbol? name) (oper-symbol? param)) (error 'parse-fundef "JYSS3: Invalid Function Definition")]
       [else (FunDefC name param (parse body))])]
    [(list 'fn (list (? symbol? name) (? symbol? param)) (? list? body))
     (cond
       [(or (oper-symbol? name) (oper-symbol? param)) (error 'parse-fundef "JYSS3: Invalid Function Definition")]
       [else (FunDefC name param (parse body))])]
    [else (error 'parse-fundef "JYSS3: Invalid Function Definition")]))

; (refactor needed)
; returns true if the given symbol is any of the following special patterns 
(define (oper-symbol? [s : Symbol]) : Boolean
  (match s
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    [other #f]))


; This function parses an expression
; Parameters: Sexp
; Return: ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? s)
     #:when (boolean? (member s '(+ * - / leq0? fn)))
     (IdC s)] 
    [(list (? symbol? f) a) (AppC f (parse a))]
    [(list (? symbol? o) l r)
     (BinopC o (parse l) (parse r))] 
    [(list 'leq0? test then else) (leq0? (parse test) (parse then) (parse else))]
    [else (error 'parse "JYSS3: Invalid Syntax")]))






; ------- Test cases

; top-interp test cases
(check-equal? (top-interp '{{fn {decr x} {leq0? x x {decr {- x 1}}}}
                            {fn {chkr y} {leq0? {+ y 0.5} {+ y -1} {* -1 y}}}
                            {fn {round z} {+ z {chkr {decr z}}}}
                            {fn {main init} {round 101.7}}
                            }
                          )
              102.0)

(check-equal? (top-interp '{{fn {decr x} {leq0? x x {decr {- x 1}}}}
                            {fn {chkr y} {leq0? {+ y 0.5} {* -1 {+ y 1}} {* -1 y}}}
                            {fn {round z} {+ z {chkr {decr z}}}}
                            {fn {main init} {round 10.3}}
                            }
                          )
              10.0)

(check-equal? (top-interp '{{fn {f x} {+ 1 x}}
                            {fn {g y} {* 1 y}}
                            {fn {main init} {+ {f 2} {g 3}}}
                            }
                          )
              6)

(check-equal? (top-interp '{{fn {f x} {+ x {* 2 {g x}}}}
                            {fn {g y} {* 7 y}}
                            {fn {ff x} {+ x x}}
                            {fn {main init} {+ {f 2} {g 3}}}
                            }
                          )
              51)

(check-equal? (top-interp '{{fn {minus-five x} {+ x {* -1 5}}}
                            {fn {main init} {minus-five {+ 8 1}}}}) 4)

(check-equal? (top-interp '{{fn {minus-five x} {+ x {* -1 5}}}
                            {fn {main init} {minus-five {+ 8 init}}}}) 3)

(check-exn (regexp (regexp-quote "parse: JYSS3: Invalid Syntax"))
           (lambda () (top-interp '{{fn {f x} {}}
                                    {fn {main init} {+ 1 2}}})))

(check-exn (regexp (regexp-quote "interp: JYSS3: Divide by Zero Error"))
           (lambda () (top-interp '{{fn {main init} {/ 1 {+ 0 0}}}})))

(check-exn (regexp (regexp-quote "interp: JYSS3: Divide by Zero Error"))
           (lambda () (top-interp '((fn (ignoreit x) (+ 3 4)) (fn (main init) (ignoreit (/ 1 (+ 0 0))))))))

(check-equal? (top-interp '{{fn {main init} {leq0? {* 3 1} 3 {+ 2 9}}}}) 11)

; interp test cases
(define test-function (list (FunDefC 'f 'x (NumC 1))))
(check-equal? (interp (BinopC '+ (NumC 1) (NumC 2)) test-function) 3)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (NumC 3)) test-function) 9)
(check-equal? (interp (BinopC '- (NumC 4) (NumC 1)) test-function) 3)
(check-equal? (interp (BinopC '/ (NumC 4) (NumC 2)) test-function) 2)

(check-equal? (interp (leq0? (NumC 1) (NumC 2) (NumC 3)) test-function) 3)
(check-equal? (interp (leq0? (NumC 0) (NumC 2) (NumC 3)) test-function) 2)
(check-equal? (interp (leq0? (NumC -10) (BinopC '/ (NumC 4) (NumC 2)) (BinopC '- (NumC 4) (NumC 1))) test-function) 2)

(check-exn (regexp (regexp-quote "interp: JYSS3: Divide by Zero Error"))
           (lambda () (interp (BinopC '/ (NumC 4) (NumC 0)) test-function)))

(check-exn (regexp (regexp-quote "interp: JYSS3: Undefined/Unreachable"))
           (lambda () (interp (IdC 'a) test-function)))


(check-exn (regexp (regexp-quote "interp: JYSS3: Divide by Zero Error"))
           (lambda () (interp (AppC 'main (NumC 0))
                              (list
                               (FunDefC 'ignoreit 'x
                                        (BinopC '+ (NumC 3) (NumC 4)))
                               (FunDefC 'main 'init (AppC 'ignoreit
                                                          (BinopC '/ (NumC 1) (BinopC '+ (NumC 0) (NumC 0)))))))))

; interp-fns test cases
(check-exn (regexp (regexp-quote "interp-fns: JYSS3: Undefined"))
           (lambda () (interp-fns'())))

(check-equal? (interp-fns
               (list
                (FunDefC 'f 'x (BinopC '+ (NumC 1) (IdC 'x)))
                (FunDefC 'g 'y (BinopC '* (NumC 1) (IdC 'y)))
                (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                             (AppC 'g (NumC 3)))))) 6)

(check-equal? (interp-fns
               (list
                (FunDefC 'f 'x (BinopC '+ (AppC 'g (IdC 'x)) (IdC 'x)))
                (FunDefC 'g 'y (BinopC '* (NumC 3) (IdC 'y)))
                (FunDefC 'main 'init (BinopC '+ (AppC 'f (NumC 2))
                                             (AppC 'g (NumC 3)))))) 17)

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
                    (FunDefC 'main 'init (AppC 'ignoreit (BinopC '/ (NumC 1) (BinopC '+ (NumC 0) (NumC 0)))))))

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
(check-exn (regexp (regexp-quote "parse-fundef: JYSS3: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {+ x} {} 6})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS3: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {+ x} 6})))
(check-exn (regexp (regexp-quote "parse-fundef: JYSS3: Invalid Function Definition"))
           (lambda () (parse-fundef '{fn {+ x} {+ x 10}})))

; oper-symbol? test cases 
(check-equal? (oper-symbol? 's) #f)
(check-equal? (oper-symbol? '+) #t)
(check-equal? (oper-symbol? '-) #t)
(check-equal? (oper-symbol? '*) #t)
(check-equal? (oper-symbol? '/) #t)

; parse test cases
(check-equal? (parse '43) (NumC 43))
(check-equal? (parse (list '+ (list '- 0 1) 2)) (BinopC '+ (BinopC '- (NumC 0) (NumC 1)) (NumC 2)))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{* {+ 1 2} 3}) (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (NumC 3)))
(check-equal? (parse '{- 4 1}) (BinopC '- (NumC 4) (NumC 1)))
(check-equal? (parse '{/ 4 2}) (BinopC '/ (NumC 4) (NumC 2)))

(check-equal? (parse '{leq0? 1 2 3}) (leq0? (NumC 1) (NumC 2) (NumC 3)))

(check-exn (regexp (regexp-quote "parse: JYSS3: Invalid Syntax"))
           (lambda () (parse '{list {list 1 2 3} 4 5 6})))

(check-exn (regexp (regexp-quote "parse: JYSS3: Invalid Syntax"))
           (lambda () (parse '{+ / 3})))

(check-exn (regexp (regexp-quote "JYSS3: Invalid Syntax"))
           (lambda () (parse '{leq0?})))

; subst test cases
(check-equal? (subst (IdC 'a) 'b (IdC 'a)) (IdC 'a))

(check-equal? (subst (NumC 0) 'init (AppC 'ignoreit (BinopC '/ (NumC 1) (BinopC '+ (NumC 0) (NumC 0)))))
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
(check-exn (regexp (regexp-quote "get-fundef: JYSS3: Unimplemented Function"))
           (lambda () (get-fundef 'a '())))
