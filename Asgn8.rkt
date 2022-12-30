;; is essentially asgn 5
;; - missing top-expand and let-stx

#lang typed/racket
(require typed/rackunit)



;; Env def 
(struct Env ([env : (Listof Binding)]) #:transparent)
(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define extend-env cons)

;; ExprC: output structure from parse (our AST)
(define-type ExprC (U NumC StrC IdC String IfC AppC LamC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AppC ([a : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)

;; Value: output structure from interp
(define-type Value (U NumV BoolV StrV PrimopV CloV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct PrimopV ([s : Symbol]) #:transparent)
(struct CloV ([arg : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

;; Top-level Env
(define top-env (Env (list
                       (Binding 'true (BoolV #t))
                       (Binding 'false (BoolV #f))
                       (Binding '+ (PrimopV '+))
                       (Binding '- (PrimopV '-))
                       (Binding '* (PrimopV '*))
                       (Binding '/ (PrimopV '/))
                       (Binding '<= (PrimopV '<=))
                       (Binding 'equal? (PrimopV 'equal?))
                       (Binding 'error (PrimopV 'error)))))

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

; returns false if any ele of the list is not a symbol 
(define (validate-args [args : (Listof Any)]) : Boolean
  (define f-list (filter (lambda (i) (equal? i #f)) (map (lambda (i) (match i
                     [(? symbol? a) #t]
                     [other #f])) args)))
  (match f-list
    ['() #t]
    [other #f]))

(define (validate-appc-expr [i : Any]) : Boolean
  (match i
    ['() #f]
    [(list a ...) #t]
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['<= #t]
    ['equal? #t]
    ['if #t]
    [(? symbol? s) #t]
    [other #f]))

;; parse a s-expression into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    
    [(? real? n) (NumC n)]

    [(? string? s) (StrC s)]
    
    [(? symbol? s) #:when (boolean? (member s '(body: if proc go))) (IdC s)]
    
    [(list 'if test then else) (IfC (parse test) (parse then) (parse else))]
    [(list 'if) (error 'parse "JYSS5: if must have a conditional clause")]
    [(list 'if test) (error 'parse "JYSS5: if and else clause must be specified in conditional")]
    [(list 'if test then) (error 'parse "JYSS5: else clause must be specified in conditional")]
    [(list 'if test then else elsee) (error 'parse "JYSS5: only can be one else clause")]
    [(? symbol? 'if) (error 'parse "JYSS5: if cannot stand alone")]

    [(list 'proc (list arguments ...) 'go e)
     (cond
       [(validate-args arguments)
        (define args (cast arguments (Listof Symbol)))
        (cond
          [(not (symbol-repeats? args)) (LamC args (parse e))]
          [else (error 'parse "JYSS5: function cannot have duplicate par names")])]
       [else (error 'parse "JYSS5: function pars are not symbols")])]

    [(list b ...) (define a (cast b (Listof Sexp)))
                  (cond
                    [(validate-appc-expr (first a))
                     (AppC (parse (first a))
                                (map (lambda (i) (parse (cast i Sexp))) (rest a)))]
                    [else (error 'parse "JYSS5: improper exprC initial syntax")])]))

;; accepts a JYSS5 Value and returns a String
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV b) (if b "true" "false")]
    [(StrV s) (~v s)]
    [(CloV a b e) "#<procedure>"]
    [(PrimopV o) "#<primop>"]))

;; lookup -- returns value associated with symbol name in an Env 
(define (lookup [for : Symbol] [env : Env]) : Value
  (match (Env-env env)
    ['() (error 'lookup "JYSS5: IdC name not found")]
    [other (cond
            [(equal? for (Binding-name (first (Env-env env)))) (Binding-val (first (Env-env env)))]
            [else
             (define rest-env (Env (rest (Env-env env))))
             (lookup for rest-env)])]))

;; combine Envs
(define (env-comb [ea : Env] [eb : Env]) : Env
  (Env (append (Env-env ea) (Env-env eb))))

;; eval a PrimopV arithmetic symbol 
(define (num-op [s : Symbol] [l : Value] [r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r))
     (cond
       [(equal? s '+) (NumV (+ (NumV-n l) (NumV-n r)))]
       [(equal? s '-) (NumV (- (NumV-n l) (NumV-n r)))]
       [(equal? s '*) (NumV (* (NumV-n l) (NumV-n r)))]
       [(equal? s '/) (if (= (NumV-n r) 0)
                          (error 'num-op "JYSS5: cannot divide by zero")
                          (NumV (/ (NumV-n l) (NumV-n r))))]
       [else (error 'num-op "JYSS5: PrimopV is not valid")])]
    [else
     (error 'num-op "JYSS5: one argument was not a number")]))

;; eval a PrimopV <= symbol 
(define (leq-op [l : Value] [r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r))
     (cond
       [(<= (NumV-n l) (NumV-n r)) (BoolV #t)]
       [else (BoolV #f)])]
    [else
     (error 'leq-op "JYSS5: one argument was not a number")]))

;; eval a PrimopV equal? symbol  
(define (eq-op [l : Value] [r : Value]) : Value
  (cond
    [(equal? l r) (BoolV #t)]
    [else (BoolV #f)]))

; define newBind, binds list of symbols to list of values in new environment
(define (newBind [sym : (Listof Symbol)] [to : (Listof Value)]) : Env
  (cond
    [(equal? (length sym) (length to)) (Env (for/list: : (Listof Binding) ([s sym]
                                                                        [val to])
                                              (Binding s val)))]
    [else (error 'interp "JYSS5 Mismatching number of arguments")]))


;; interp 
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(NumC n) (NumV n)]

    [(StrC s) (StrV s)]
    
    [(IdC n) (lookup n env)]

    [(LamC a b) (CloV a b env)]
    
    [(AppC e (list arguments ...)) (define args (cast arguments (Listof ExprC)))
                                   (define arg-values
                                     (map (lambda (i) (define ii (cast i ExprC)) (interp ii env)) args))
                                   (define ee (interp e env))
                                   (match ee
                                     [(CloV a b e) (define f-value (cast ee CloV))
                                                   (cond
                                                     [(equal? (length args) (length (CloV-arg f-value)))
                                                      (interp (CloV-body f-value)
                                                              (Env
                                                               (append
                                                                (Env-env (newBind (CloV-arg f-value) arg-values))
                                                                           (Env-env e))))]
                                                     [else (error 'interp "JYSS5: AppC with wrong # args")])]
                                     [(PrimopV p) (cond
                                                    [(equal? p '<=) (leq-op (interp (first args) env)
                                                                            (interp (first (rest args)) env))]
                                                    [(equal? p 'equal?) (eq-op (interp (first args) env)
                                                                               (interp (first (rest args)) env))]
                                                    [(equal? p 'error) (error 'interp "JYSS5: user-error")]
                                                    [else (num-op p (interp (first args) env)
                                                                  (interp (first (rest args)) env))])]
                                     [other ee])]

    [(IfC test then otw) (define res (interp test env))
                         (match res
                           [(BoolV b) (cond
                                        [b (interp then env)]
                                        [else (interp otw env)])]
                           [other (error 'interp "JYSS5: conditional body did not eval to a boolean")])]))

;; top/main func
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))



;; ------ test cases
(check-equal? (top-interp '(+ 1 2)) "3")

; symbol-repeats? 
(check-equal? (symbol-repeats? '()) #f)
(check-equal? (symbol-repeats? (list 'a)) #f)
(check-equal? (symbol-repeats? (list 'a 'b 'c 'd 'asdf)) #f)
(check-equal? (symbol-repeats? (list 'a 'a 'c 'a 'asdf)) #t)
(check-equal? (symbol-repeats? (list 'test 'test)) #t)
(check-equal? (symbol-repeats? (list 'x 'y 'y 'x)) #t)
(check-equal? (symbol-repeats? (list 'x 'y 'a 'x)) #t)
(check-equal? (symbol-repeats? (list 'a 'b 'c 'c)) #t)

; parse
(check-equal? (parse '168) (NumC 168))
(check-equal? (parse '"one") (StrC "one"))
(check-equal? (parse 'a) (IdC 'a))
(check-equal? (parse '{if 1 a b}) (IfC (NumC 1) (IdC 'a) (IdC 'b)))
(check-equal? (parse '{if g a b}) (IfC (IdC 'g) (IdC 'a) (IdC 'b)))
(check-exn (regexp (regexp-quote "JYSS5: else clause must be specified in conditional"))
           (lambda () (parse '{if g a})))
(check-exn (regexp (regexp-quote "JYSS5: if and else clause must be specified in conditional"))
           (lambda () (parse '{if g})))
(check-exn (regexp (regexp-quote "JYSS5: if must have a conditional clause"))
           (lambda () (parse '{if})))
(check-exn (regexp (regexp-quote "JYSS5: only can be one else clause"))
           (lambda () (parse '{if a b c d})))
(check-equal? (parse '{proc {z y} go {+ z y}}) (LamC (list 'z 'y) (AppC (IdC '+) (list (IdC 'z) (IdC 'y)))))
(check-equal? (parse '{proc {z y} go 1}) (LamC (list 'z 'y) (NumC 1)))
(check-exn (regexp (regexp-quote "JYSS5: function cannot have duplicate par names"))
           (lambda () (parse '{proc {z y y} go 1})))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{<= 10 {equal? 1 2}})
              (AppC (IdC '<=) (list (NumC 10) (AppC (IdC 'equal?) (list (NumC 1) (NumC 2))))))
(check-exn (regexp (regexp-quote "JYSS5: if cannot stand alone"))
           (lambda () (parse '{+ if 4})))
(check-exn (regexp (regexp-quote "JYSS5: function pars are not symbols"))
           (lambda () (parse '{proc (3 4 5) go 6})))
(check-exn (regexp (regexp-quote "JYSS5: improper exprC initial syntax"))
           (lambda () (parse '{3 4 5})))



; serialize
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "test")) "\"test\"")
(check-equal? (serialize (PrimopV 'equal?)) "#<primop>")
(check-equal? (serialize (CloV (list 'a) (NumC 2) top-env)) "#<procedure>")

; lookup
(check-equal? (lookup 'x (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 2))))) (NumV 2))
(check-exn (regexp (regexp-quote "JYSS5: IdC name not found"))
           (lambda () (lookup 'x (Env '()))))
(check-exn (regexp (regexp-quote "JYSS5: IdC name not found"))
           (lambda () (lookup 'x (Env (list (Binding 'a (NumV 1)))))))

; env-comb
(check-equal? (env-comb (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))
                        (Env (list (Binding 'b (BoolV #t)) (Binding 'g (NumV 1)))))
              (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22)) (Binding 'b (BoolV #t)) (Binding 'g (NumV 1)))))

; num-op
(check-equal? (num-op '+ (NumV 3) (NumV 20)) (NumV 23))
(check-equal? (num-op '- (NumV 3) (NumV 20)) (NumV -17))
(check-equal? (num-op '* (NumV 3) (NumV 20)) (NumV 60))
(check-equal? (num-op '/ (NumV 60) (NumV 20)) (NumV 3))
(check-exn (regexp (regexp-quote "JYSS5: cannot divide by zero"))
           (lambda () (num-op '/ (NumV 60) (NumV 0))))
(check-exn (regexp (regexp-quote "JYSS5: PrimopV is not valid"))
           (lambda () (num-op 'ab (NumV 60) (NumV 0))))
(check-exn (regexp (regexp-quote "JYSS5: one argument was not a number"))
           (lambda () (num-op 'ab (NumV 60) (BoolV #t))))

; leq-op
(check-equal? (leq-op (NumV 10) (NumV 11)) (BoolV #t))
(check-equal? (leq-op (NumV 10) (NumV 10)) (BoolV #t))
(check-equal? (leq-op (NumV 10) (NumV 3)) (BoolV #f))
(check-exn (regexp (regexp-quote "JYSS5: one argument was not a number"))
           (lambda () (leq-op (NumV 10) (BoolV #t))))

; validate-appc-expr
(check-equal? (validate-appc-expr '()) #f)
(check-equal? (validate-appc-expr '+) #t)
(check-equal? (validate-appc-expr '-) #t)
(check-equal? (validate-appc-expr '*) #t)
(check-equal? (validate-appc-expr '/) #t)
(check-equal? (validate-appc-expr '<=) #t)
(check-equal? (validate-appc-expr 'equal?) #t)
(check-equal? (validate-appc-expr 'if) #t)
(check-equal? (validate-appc-expr 31) #f)
(check-equal? (validate-appc-expr (list 32)) #t)
(check-equal? (validate-appc-expr 'b) #t)

; eq-op
(check-equal? (eq-op (StrV "ok") (StrV "ok")) (BoolV #t))
(check-equal? (eq-op (StrV "ok") (NumV 3)) (BoolV #f))
(check-equal? (eq-op (NumV 10) (NumV 10)) (BoolV #t))

; validate-args
(check-equal? (validate-args (list 'a 'b 'c 'eef)) #t)
(check-equal? (validate-args (list 'a 'b 2 'eef)) #f)


; newBind
(check-equal? (newBind (list 'a 'b 'c) (list (NumV 1) (NumV 2) (NumV 3)))
              (Env (list (Binding 'a (NumV 1))
                         (Binding 'b (NumV 2))
                         (Binding 'c (NumV 3)))))
(check-exn (regexp (regexp-quote "JYSS5 Mismatching number of arguments"))
           (lambda () (newBind (list 'a 'b 'c) (list (NumV 2)))))

; interp
(check-equal? (interp (NumC 4) top-env) (NumV 4))
(check-equal? (interp (StrC "one") top-env) (StrV "one"))
(check-equal? (interp (IdC 'x) (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))) (NumV 22))
(check-equal? (interp (IdC '+) top-env) (PrimopV '+))
(check-equal? (interp (IdC '/) top-env) (PrimopV '/))
(check-equal? (interp (IdC 'equal?) top-env) (PrimopV 'equal?))
(check-equal? (interp (IdC '<=) top-env) (PrimopV '<=))
(check-exn (regexp (regexp-quote "JYSS5: IdC name not found"))
           (lambda () (interp (IdC 'x) top-env)))
(check-equal? (interp (AppC (LamC (list 'x) (IdC 'x)) (list (IdC 'x)))
                      (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))) (NumV 22))
(check-equal? (interp (AppC (LamC (list 'x 'y) (IdC 'x)) (list (IdC 'x) (IdC 'y)))
                      (Env (list (Binding 'y (NumV 1)) (Binding 'x (NumV 22))))) (NumV 22))
(check-equal? (interp (AppC (NumC 4) (list (IdC 'x)))
                      (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))) (NumV 4))
(check-equal? (interp (AppC (IdC 'x) (list (IdC 'x)))
                      (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))) (NumV 22))
(check-equal? (interp (AppC (IdC '+) (list (IdC 'x) (NumC 1)))
                      (env-comb top-env (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))))
              (NumV 23))
(check-equal? (interp (AppC (IdC '-) (list (IdC 'x) (NumC 1)))
                      (env-comb top-env (Env (list (Binding 'a (NumV 1)) (Binding 'x (NumV 22))))))
              (NumV 21))
(check-equal? (interp (AppC (IdC '+) (list (IdC 'x) (IdC 'z)))
                      (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22))))))
              (NumV 67))
(check-equal? (interp (AppC (IdC '<=) (list (IdC 'x) (IdC 'z)))
                      (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22))))))
              (BoolV #t))
(check-equal? (interp (AppC (IdC '<=) (list (IdC 'z) (IdC 'x)))
                      (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22))))))
              (BoolV #f))
(check-equal? (interp (AppC (IdC 'equal?) (list (IdC 'z) (IdC 'x)))
                      (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22))))))
              (BoolV #f))
(check-equal? (interp (AppC (IdC 'equal?) (list (IdC 'z) (IdC 'x)))
                      (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 45))))))
              (BoolV #t))
(check-equal? (interp (AppC (IdC 'equal?) (list (IdC 'z) (IdC 'x)))
                      (env-comb top-env (Env (list (Binding 'z (StrV "ok")) (Binding 'x (StrV "ok"))))))
              (BoolV #t))
(check-exn (regexp (regexp-quote "JYSS5: user-error"))
           (lambda () (interp (AppC (IdC 'error) '()) top-env)))
(check-equal? (interp (LamC (list 'x 'z) (AppC (IdC '+) (list (IdC 'x) (IdC 'z))))
                      (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22))))))
              (CloV (list 'x 'z) (AppC (IdC '+) (list (IdC 'x) (IdC 'z)))
                    (env-comb top-env (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22)))))))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 10) (NumC 11)))
                           (NumC 1) (NumC 2)) top-env) (NumV 1))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (IdC 'x) (IdC 'z)))
                           (NumC 1) (NumC 2))
                      (env-comb top-env
                                (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22)))))) (NumV 1))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (IdC 'z) (IdC 'x)))
                           (NumC 1) (NumC 2))
                      (env-comb top-env
                                (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22)))))) (NumV 2))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 10) (NumC 11)))
                           (AppC (IdC '<=) (list (IdC 'x) (IdC 'z))) (NumC 2))
                      (env-comb top-env
                                (Env (list (Binding 'z (NumV 45)) (Binding 'x (NumV 22)))))) (BoolV #t))
(check-exn (regexp (regexp-quote "JYSS5: conditional body did not eval to a boolean"))
           (lambda () (interp (IfC (AppC (IdC '+) (list (NumC 10) (NumC 11)))
                           (AppC (IdC '<=) (list (IdC 'x) (IdC 'z))) (NumC 2))
                      top-env)))
(check-exn (regexp (regexp-quote "JYSS5: AppC with wrong # args"))
           (lambda () (interp (AppC (LamC (list 'x 'y 'z) (NumC 1)) '()) top-env)))

