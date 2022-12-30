#lang typed/racket
(require typed/rackunit)


;; Full project implemented





;; ExprC
(define-type ExprC (U NumC StrC AppC IdC IfC
                      LamC RecC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([params : (Listof ArgC)] [body : ExprC]) #:transparent)
(struct ArgC ([a : Symbol] [t : Ty]) #:transparent)
(struct RecC ([name : Symbol] [lam : RecsLamC] [body : ExprC]) #:transparent)
(struct RecsLamC ([params : (Listof ArgC)] [out-ty : Ty] [body : ExprC]) #:transparent)

;; Value 
(define-type Value (U NumV BoolV StrV PrimopV CloV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct PrimopV ([s : Symbol]) #:transparent)
(struct CloV ([arg : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

;; Value Env 
(struct Binding ([name : Symbol] [val : (Boxof Value)]) #:transparent)
(define-type Env (Listof Binding))
(define extend-env cons)
(define base-env (list
                       (Binding 'true (box (BoolV #t)))
                       (Binding 'false (box (BoolV #f)))
                       (Binding '+ (box (PrimopV '+)))
                       (Binding '- (box (PrimopV '-)))
                       (Binding '* (box (PrimopV '*)))
                       (Binding '/ (box (PrimopV '/)))
                       (Binding '<= (box (PrimopV '<=)))
                       (Binding 'num-eq? (box (PrimopV 'num-eq?)))
                       (Binding 'str-eq? (box (PrimopV 'str-eq?)))
                       (Binding 'substring (box (PrimopV 'substring)))))

;; Type
(define-type Ty (U NumT BoolT StrT FunT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StrT () #:transparent)
(struct FunT ([i : (Listof Ty)] [o : Ty]) #:transparent)

;; Type Env
(struct TBinding ([name : Symbol] [type : Ty]) #:transparent)
(define-type Tenv (Listof TBinding))
(define extend-ty-env cons)
(define base-tenv (list
                        (TBinding `+ (FunT (list (NumT) (NumT)) (NumT)))
                        (TBinding `- (FunT (list (NumT) (NumT)) (NumT)))
                        (TBinding `/ (FunT (list (NumT) (NumT)) (NumT)))
                        (TBinding `* (FunT (list (NumT) (NumT)) (NumT)))
                        (TBinding `true (BoolT))
                        (TBinding `false (BoolT))
                        (TBinding `<= (FunT (list (NumT) (NumT)) (BoolT)))
                        (TBinding `num-eq?  (FunT (list (NumT) (NumT)) (BoolT)))
                        (TBinding `str-eq? (FunT (list (StrT) (StrT)) (BoolT)))
                        (TBinding `substring (FunT (list (StrT) (NumT) (NumT)) (StrT)))))





;; looks up the given symbol in the given env and returns associated Value 
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "JYSS7 IdC name not found")]
    [other (cond
            [(equal? for (Binding-name (first env))) (unbox (Binding-val (first env)))]
            [else
             (define rest-env (rest env))
             (lookup for rest-env)])]))

;; evals a PrimopV arithmetic symbol 
(define (num-op [s : Symbol] [l : Value] [r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r))
     (cond
       [(equal? s '+) (NumV (+ (NumV-n l) (NumV-n r)))]
       [(equal? s '-) (NumV (- (NumV-n l) (NumV-n r)))]
       [(equal? s '*) (NumV (* (NumV-n l) (NumV-n r)))]
       [(equal? s '/) (if (= (NumV-n r) 0)
                          (error 'num-op "JYSS7 cannot divide by zero")
                          (NumV (/ (NumV-n l) (NumV-n r))))]
       [else (error 'num-op "JYSS7 PrimopV is not valid")])]
    [else
     (error 'num-op "JYSS7 one argument was not a number")]))

;; eval a PrimopV <= symbol 
(define (leq-op [l : Value] [r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r))
     (cond
       [(<= (NumV-n l) (NumV-n r)) (BoolV #t)]
       [else (BoolV #f)])]
    [else
     (error 'leq-op "JYSS7 one argument was not a number")]))

;; eval a PrimopV equal? symbol  
(define (eq-op [l : Value] [r : Value]) : Value
  (cond
    [(equal? l r) (BoolV #t)]
    [else (BoolV #f)]))

;; eval a PrimopV substring symbol 
(define (substring-op [s : Value] [i : Value] [j : Value]) : Value
  (cond
    [(and (StrV? s) (and (NumV? i) (NumV? j)))
     (StrV (substring (StrV-s s) (cast (NumV-n i) Integer) (cast (NumV-n j) Integer)))]
    [else (error 'substring-op "JYSS7 wrong substring args given")]))

;; turns AST into answers 
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(NumC n) (NumV n)]

    [(StrC s) (StrV s)]
    
    [(IdC n) (lookup n env)]

    [(LamC params body) (CloV (map (lambda (i) (ArgC-a i)) params) body env)]
    
    [(AppC fun (list arguments ...))
     (define args (cast arguments (Listof ExprC)))
     (define arg-values 
       (map (lambda (i) (define ii (cast i ExprC)) (interp ii env)) args))
     (define f-value (interp fun env))
     (match f-value
       [(CloV args body ev)
           (cond
             [(equal? (length arg-values) (length (CloV-arg f-value)))
              (define new-bindings
                (for/list: : (Listof Binding) ([s (CloV-arg f-value)]
                                               [val arg-values]) (Binding s (box val))))
              (define extended-env (append env new-bindings))
              (interp (CloV-body f-value) extended-env)]
             [else (error 'interp "JYSS7 Mismatching number of arguments")])]
       [(PrimopV p) (cond
                      [(equal? p '<=) (leq-op (interp (first args) env)
                                              (interp (first (rest args)) env))]
                      [(or (equal? p 'num-eq?) (equal? p 'str-eq?))
                       (eq-op (interp (first args) env) (interp (first (rest args)) env))]
                      [(equal? p 'substring) (substring-op
                                              (interp (first args) env)
                                              (interp (first (rest args)) env)
                                              (interp (first (rest (rest args))) env))]
                      [else (num-op p (interp (first args) env)
                                    (interp (first (rest args)) env))])])]

    [(IfC test then otw) (define res (interp test env))
                         (match res
                           [(BoolV b) (cond
                                        [b (interp then env)]
                                        [else (interp otw env)])]
                           [other (error 'interp "JYSS7 conditional body did not eval to a boolean")])]

    [(RecC name lam body) (define b (box (cast (StrV "junk") Value)))
                          (define extended-env (append env (list (Binding name b))))
                          (define args (map (lambda (i) (ArgC-a i)) (RecsLamC-params lam)))
                          (define new-clov (CloV args body extended-env))
                          (set-box! b new-clov)
                          (interp body extended-env)]))

;; returns Type associated with a symbol in a Type Env
(define (lookup-type [for : Symbol] [env : Tenv]) : Ty
  (match env
    ['() (error 'lookupVal "JYSS7 IdC name not found")]
    [other (cond
            [(equal? for (TBinding-name (first env))) (TBinding-type (first env))]
            [else
             (define rest-env (rest env))
             (lookup-type for rest-env)])]))

;; checks the types in the parsed AST are correct
(define (type-check [e : ExprC] [env : Tenv]) : Ty
  (match e
    [(NumC n) (NumT)]
    [(StrC s) (StrT)]
    [(IdC n) (lookup-type n env)]
    [(AppC fun args) (define fun-type (type-check fun env))
                     (cond
                       [(not (FunT? fun-type)) (error 'type-check "JYSS7 type error")]
                       [else (define arg-types (map (lambda (i) (type-check (cast i ExprC) env)) args))
                             (if (equal? (FunT-i fun-type) arg-types)
                             (FunT-o fun-type)
                             (error 'type-check "JYSS7 type error"))])]
    [(IfC test then else) (define test-type (type-check test env))
                          (cond
                            [(not (BoolT? test-type)) (error 'type-check "JYSS7 type error")]
                            [else (define then-type (type-check then env))
                                  (define else-type (type-check else env))
                                  (if (equal? then-type else-type)
                                      then-type
                                      (error 'type-check "JYSS7 type error"))])]

    [(LamC params body)
     (define new-bindings (map (lambda (i)
         (define ii (cast i ArgC))
         (TBinding (ArgC-a ii) (ArgC-t ii))) params))
     (define extended-env (append env new-bindings))
     (define param-types (cast (map (lambda (i) (ArgC-t i)) params) (Listof Ty)))
     (FunT param-types (type-check body extended-env))]

    [(RecC name lam body)
     (define parname-bindings (map (lambda (i)
         (define ii (cast i ArgC))
         (TBinding (ArgC-a ii) (ArgC-t ii))) (RecsLamC-params lam)))
     (define recname-binding (list (TBinding name (RecsLamC-out-ty lam))))
     (define extended-env (append env parname-bindings recname-binding))
     (cond
       [(equal? (RecsLamC-out-ty lam) (type-check (RecsLamC-body lam) extended-env))
        (define param-types (cast (map (lambda (i) (ArgC-t i)) (RecsLamC-params lam)) (Listof Ty)))
        (define out-type (RecsLamC-out-ty lam))
        (FunT param-types out-type)]
       [else (error 'type-check "JYSS7 type error")])]))

;; parses S-expressions into Types
(define (parse-type [s : Sexp]) : Ty
  (match s
    [`num (NumT)]
    [`str (StrT)]
    [`bool (BoolT)]
    [(list t1 ... `-> t2) (FunT (map (lambda ([x : Sexp]) (parse-type x)) (cast t1 (Listof Sexp))) (parse-type t2))]
    [other (error 'parse-type "JYSS7 Invalid type")]))

;; parses a par:type pair into an ArgC struct 
;; ex. '(x : bool) -> (ArgC 'x (BoolT)) 
(define (parse-argc [l : Sexp]) : ArgC
  (match l
    [(list a ': t) (ArgC (cast a Symbol) (parse-type (cast t Sexp)))]
    [other (error 'parse-argc "JYSS7 incorrect par syntax, requires [par : type]")]))

;; checks if a list of symbols has any identical symbols
;; returns true if it does, false otherwise 
(define (check-duplicates [pars : (Listof Symbol)]) : Boolean
  (match pars
    ['() #f]
    [(cons f '()) #f]
    [(cons f r) (cond
                  [(eq? f (first r)) #t]
                  [(check-duplicates (cons f (rest r))) #t]
                  [(check-duplicates r) #t]
                  [else #f])]))

;; parses JYSS7 syntax into AST 
(define (parse [s : Sexp]) : ExprC
  (match s
    [`() (error 'parse "JYSS7 Empty S-exp")]
    [(? real? r) (NumC r)]
    [(? string? s) (StrC s)]
    [(? symbol? s) (cond
                     [(member s `(if : = var in rec => ->)) (error 'parse "JYSS7 Unexpected symbol")]
                     [else (IdC s)])]
    
    [(list 'if if then else) (IfC (parse if) (parse then) (parse else))]

    [(list 'proc (list a ...) 'go body)
     (define params (cast (map (lambda (i) (parse-argc i)) a) (Listof ArgC)))
     (define param-names (cast (map (lambda (i) (ArgC-a i)) params) (Listof Symbol)))
     (if (not (check-duplicates param-names))
        (LamC params (parse (cast body Sexp)))
        (error 'parse "JYSS7 duplicates"))]

    [(list 'vars:
           (list a ... '= exprs) ...
           'body: body)
     (define ids (cast a (Listof Sexp)))
     (define params (cast (map (lambda (i) (parse-argc i)) ids) (Listof ArgC)))
     (define param-names (cast (map (lambda (i) (ArgC-a i)) params) (Listof Symbol)))
     (if (not (check-duplicates param-names))
        (AppC (LamC params (parse (cast body Sexp)))
              (map (lambda (i) (parse (cast i Sexp))) exprs))
        (error 'parse "JYSS7 duplicates"))]

    [(list 'rec: (list id '= (list 'proc (list a ...) ': o-type 'go bodya)) 'in bodyb)
     (define params (cast (map (lambda (i) (parse-argc i)) a) (Listof ArgC)))
     (define param-names (cast (map (lambda (i) (ArgC-a i)) params) (Listof Symbol)))
     (if (not (check-duplicates param-names))
        (RecC (cast id Symbol)
              (RecsLamC params (parse-type (cast o-type Sexp)) (parse (cast bodya Sexp)))
              (parse (cast bodyb Sexp)))
        (error 'parse "JYSS7 duplicates"))]
    
    [(list l ...)
        (AppC (parse (first l))
              (cast (map (lambda ([x : Sexp]) (parse x)) (cast (rest l) (Listof Sexp))) (Listof ExprC)))]))

;; takes in a value input and returns a string representation 
(define (serialize [output : Value]) : String
  (match output
    [(NumV n) (~v n)]
    [(BoolV b) (if b "true" "false")]
    [(StrV s) (~v s)]
    [(CloV a b e) "#<procedure>"]
    [(PrimopV o) "#<primop>"]))

;; main function --- parses, type checks, interprets
(define (top-interp [s : Sexp]) : String 
  (define ast (parse s))
  (type-check ast base-tenv)
  (serialize (interp ast base-env)))





;; ----- test cases
;; top-interp
(check-equal? (top-interp '{+ 1 2}) "3")

;; serialize
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "test")) "\"test\"")
(check-equal? (serialize (PrimopV 'equal?)) "#<primop>")
(check-equal? (serialize (CloV (list 'a) (NumC 2) base-env)) "#<procedure>")

;; parse
(check-exn (regexp (regexp-quote "JYSS7 Empty S-exp")) 
           (lambda () (parse '())))
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse "abc") (StrC "abc"))
(check-equal? (parse 'a) (IdC 'a))
(check-exn (regexp (regexp-quote "JYSS7 Unexpected symbol")) 
           (lambda () (parse '(=))))
(check-equal? (parse '{if a b c}) (IfC (IdC 'a) (IdC 'b) (IdC 'c)))
(check-equal? (parse '{proc {[x : num] [y : str] [z : bool]} go 10})
              (LamC (list (ArgC 'x (NumT)) (ArgC 'y (StrT)) (ArgC 'z (BoolT))) (NumC 10)))
(check-exn (regexp (regexp-quote "JYSS7 duplicates")) 
           (lambda () (parse '{vars:
                               [x : num = 10]
                               [y : str = "abc"]
                               [x : num = 11]
                               body:
                               12})))
(check-exn (regexp (regexp-quote "JYSS7 duplicates")) 
           (lambda () (parse '{proc {[x : num] [x : str]} go 1})))
(check-equal? (parse '{a b c}) (AppC (IdC 'a) (list (IdC 'b) (IdC 'c))))
(check-equal? (parse '{vars:
                       [x : num = 10]
                       [y : str = "abc"]
                       body: 12})
              (AppC (LamC (list (ArgC 'x (NumT)) (ArgC 'y (StrT))) (NumC 12)) (list (NumC 10) (StrC "abc"))))
(check-equal? (parse '{vars:
                       [x : num = 10]
                       [z : (num -> num) = "abc"]
                       body: (a 12)})
              (AppC (LamC (list (ArgC 'x (NumT)) (ArgC 'z (FunT (list (NumT)) (NumT))))
                          (AppC (IdC 'a) (list (NumC 12))))
                    (list (NumC 10) (StrC "abc"))))
(check-equal? (parse '{rec: [my-func = {proc {[x : num] [y : str] [z : bool]} : bool go 1}] in 4})
              (RecC 'my-func (RecsLamC (list (ArgC 'x (NumT)) (ArgC 'y (StrT))
                                             (ArgC 'z (BoolT))) (BoolT) (NumC 1)) (NumC 4)))
(check-exn (regexp (regexp-quote "JYSS7 duplicates")) 
           (lambda () (parse '{rec: [my-func = {proc {[x : num] [x : str] [z : bool]} : bool go 1}] in 4})))

;; check-duplicates
(check-equal? (check-duplicates '()) #f)
(check-equal? (check-duplicates (list 'a)) #f)
(check-equal? (check-duplicates (list 'a 'a)) #t)
(check-equal? (check-duplicates (list 'a 'b)) #f)
(check-equal? (check-duplicates (list 'a 'b 'c)) #f)
(check-equal? (check-duplicates '()) #f)
(check-equal? (check-duplicates (list 'a)) #f)
(check-equal? (check-duplicates (list 'a 'b 'c 'd 'asdf)) #f)
(check-equal? (check-duplicates (list 'a 'a 'c 'a 'asdf)) #t)
(check-equal? (check-duplicates (list 'test 'test)) #t)
(check-equal? (check-duplicates (list 'x 'y 'y 'x)) #t)
(check-equal? (check-duplicates (list 'x 'y 'a 'x)) #t)
(check-equal? (check-duplicates (list 'a 'b 'c 'c)) #t)

;; parse-argc
(check-equal? (parse-argc '(x : bool)) (ArgC 'x (BoolT)))
(check-exn (regexp (regexp-quote "JYSS7 incorrect par syntax, requires [par : type]")) 
           (lambda () (parse-argc '{a b : c})))

;; parse-type
(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type '{num str -> bool}) (FunT (list (NumT) (StrT)) (BoolT)))
(check-equal? (parse-type '{-> num}) (FunT '() (NumT)))
(check-exn (regexp (regexp-quote "JYSS7 Invalid type")) 
           (lambda () (parse-type '(=))))

;; type-check
(check-equal? (type-check (RecC 'f (RecsLamC (list (ArgC 'n (StrT))) (NumT) (NumC 3))
                                (AppC (LamC (list (ArgC 'ff (FunT (list (StrT)) (NumT))))
                                            (AppC (IdC 'ff) (list (StrC "gg"))))
                                      (list (LamC (list (ArgC 'n (StrT))) (AppC (IdC 'f) (list (StrC "abc")))))))
                          base-tenv)
              (FunT (list (StrT)) (NumT)))
(check-exn (regexp (regexp-quote "JYSS7 type error")) 
           (lambda () (type-check (RecC 'f (RecsLamC (list (ArgC 'n (StrT))) (BoolT) (NumC 3))
                                (AppC (LamC (list (ArgC 'ff (FunT (list (StrT)) (StrT))))
                                            (AppC (IdC 'ff) (list (StrC "gg"))))
                                      (list (LamC (list (ArgC 'n (StrT))) (AppC (IdC 'f) (list (StrC "abc")))))))
                          base-tenv)))
(check-equal? (type-check (LamC (list (ArgC 'x (NumT)) (ArgC 'y (BoolT))) (NumC 10)) base-tenv)
             (FunT (list (NumT) (BoolT)) (NumT)))
(check-equal? (type-check (NumC 10) base-tenv) (NumT))
(check-equal? (type-check (StrC "abcd") base-tenv) (StrT))
(check-equal? (type-check (IdC '-) base-tenv) (FunT (list (NumT) (NumT)) (NumT)))
(check-equal? (type-check (AppC (IdC '+) (list (NumC 4) (NumC 10))) base-tenv)
              (NumT))
(check-exn (regexp (regexp-quote "JYSS7 type error")) 
           (lambda () (type-check (AppC (IdC 'true) (list (NumC 4) (NumC 10))) base-tenv)))
(check-exn (regexp (regexp-quote "JYSS7 type error")) 
           (lambda () (type-check (AppC (IdC '*) (list (NumC 4) (StrC "abc"))) base-tenv)))
(check-equal? (type-check (IfC (AppC (IdC '<=) (list (NumC 10) (NumC 11)))
                               (StrC "abc")
                               (StrC "def")) base-tenv)
              (StrT))
(check-equal? (type-check (IfC (AppC (IdC 'num-eq?) (list (NumC 10) (NumC 11)))
                               (NumC 1)
                               (NumC 2)) base-tenv)
              (NumT))
(check-exn (regexp (regexp-quote "JYSS7 type error")) 
           (lambda () (type-check (IfC (AppC (IdC '+) (list (NumC 10) (NumC 11)))
                               (NumC 1)
                               (NumC 2)) base-tenv)))
(check-exn (regexp (regexp-quote "JYSS7 type error")) 
           (lambda () (type-check (IfC (AppC (IdC 'substring) (list (NumC 10) (NumC 11)))
                               (NumC 1)
                               (NumC 2)) base-tenv)))
(check-exn (regexp (regexp-quote "JYSS7 type error")) 
           (lambda () (type-check (IfC (AppC (IdC '<=) (list (NumC 10) (NumC 11)))
                               (StrC "a")
                               (NumC 2)) base-tenv)))

;; lookup-type
(check-exn (regexp (regexp-quote "JYSS7 IdC name not found")) 
           (lambda () (lookup-type '+ '())))
(check-equal? (lookup-type '+ base-tenv) (FunT (list (NumT) (NumT)) (NumT)))
(check-equal? (lookup-type 'str-eq? base-tenv) (FunT (list (StrT) (StrT)) (BoolT)))

;; interp
(check-equal? (interp (RecC 'f (RecsLamC (list (ArgC 'x (NumT))) (NumT) (NumC 1)) (NumC 2)) base-env) (NumV 2))
(check-exn (regexp (regexp-quote "JYSS7 Mismatching number of arguments")) 
           (lambda ()
             (interp (AppC (LamC (list (ArgC 'x (NumT))) (NumC 10)) (list (NumC 1) (NumC 2))) base-env)))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 10) (NumC 11)))
                           (NumC 10)
                           (NumC 11)) base-env) (NumV 10))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 10) (NumC 5)))
                           (NumC 10)
                           (NumC 11)) base-env) (NumV 11))
(check-exn (regexp (regexp-quote "JYSS7 conditional body did not eval to a boolean")) 
           (lambda () (interp (IfC (AppC (IdC '+) (list (NumC 10) (NumC 11)))
                           (NumC 10)
                           (NumC 11)) base-env)))
(check-equal? (interp (AppC (IdC '<=) (list (NumC 10) (NumC 11))) base-env) (BoolV #t))
(check-equal? (interp (AppC (IdC 'num-eq?) (list (NumC 10) (NumC 11))) base-env) (BoolV #f))
(check-equal? (interp (AppC (IdC 'substring) (list (StrC "abcd") (NumC 0) (NumC 2))) base-env) (StrV "ab"))
(check-equal? (interp (AppC (IdC '+) (list (NumC 10) (NumC 11))) base-env) (NumV 21))
(check-equal? (interp (StrC "abc") base-env) (StrV "abc"))
(check-equal? (interp (AppC (LamC '() (NumC 10)) '()) base-env) (NumV 10))
(check-equal? (interp (AppC (LamC (list (ArgC 'x (NumT))) (NumC 10)) (list (NumC 10))) base-env) (NumV 10))
(check-equal? (interp (LamC (list (ArgC 'x (NumT)) (ArgC 'z (NumT))) (AppC (IdC '+) (list (IdC 'x) (IdC 'z))))
                      (append base-env (list (Binding 'z (box (NumV 45))) (Binding 'x (box (NumV 22))))))
              (CloV (list 'x 'z) (AppC (IdC '+) (list (IdC 'x) (IdC 'z)))
                    (append base-env (list (Binding 'z (box (NumV 45))) (Binding 'x (box (NumV 22)))))))
(check-equal? (interp (NumC 4) base-env) (NumV 4))
(check-equal? (interp (IdC 'x) (list (Binding 'a (box (NumV 1))) (Binding 'x (box (NumV 22))))) (NumV 22))
(check-equal? (interp (IdC '+) base-env) (PrimopV '+))
(check-equal? (interp (IdC '/) base-env) (PrimopV '/))
(check-equal? (interp (IdC '<=) base-env) (PrimopV '<=))
(check-equal? (interp (LamC (list (ArgC 'x (NumT)) (ArgC 'y (BoolT))) (NumC 10)) base-env)
              (CloV (list 'x 'y) (NumC 10) base-env))

;; substring-op
(check-equal? (substring-op (StrV "abcd") (NumV 1) (NumV 3)) (StrV "bc"))
(check-exn (regexp (regexp-quote "JYSS7 wrong substring args given"))
           (lambda () (substring-op (NumV 1) (NumV 1) (NumV 2))))

;; eq-op
(check-equal? (eq-op (StrV "ok") (StrV "ok")) (BoolV #t))
(check-equal? (eq-op (StrV "ok") (NumV 3)) (BoolV #f))
(check-equal? (eq-op (NumV 10) (NumV 10)) (BoolV #t))

;; leq-op
(check-equal? (leq-op (NumV 10) (NumV 11)) (BoolV #t))
(check-equal? (leq-op (NumV 10) (NumV 10)) (BoolV #t))
(check-equal? (leq-op (NumV 10) (NumV 3)) (BoolV #f))
(check-exn (regexp (regexp-quote "JYSS7 one argument was not a number"))
           (lambda () (leq-op (NumV 10) (BoolV #t))))

;; num-op
(check-equal? (num-op '+ (NumV 3) (NumV 20)) (NumV 23))
(check-equal? (num-op '- (NumV 3) (NumV 20)) (NumV -17))
(check-equal? (num-op '* (NumV 3) (NumV 20)) (NumV 60))
(check-equal? (num-op '/ (NumV 60) (NumV 20)) (NumV 3))
(check-exn (regexp (regexp-quote "JYSS7 cannot divide by zero"))
           (lambda () (num-op '/ (NumV 60) (NumV 0))))
(check-exn (regexp (regexp-quote "JYSS7 PrimopV is not valid"))
           (lambda () (num-op 'ab (NumV 60) (NumV 0))))
(check-exn (regexp (regexp-quote "JYSS7 one argument was not a number"))
           (lambda () (num-op 'ab (NumV 60) (BoolV #t))))

;; lookup
(check-equal? (lookup 'x (list (Binding 'a (box (NumV 1))) (Binding 'x (box (NumV 2))))) (NumV 2))
(check-exn (regexp (regexp-quote "JYSS7 IdC name not found"))
           (lambda () (lookup 'x '())))
(check-exn (regexp (regexp-quote "JYSS7 IdC name not found"))
           (lambda () (lookup 'x (list (Binding 'a (box (NumV 1)))))))
(check-equal? (lookup '+ base-env) (PrimopV '+))
