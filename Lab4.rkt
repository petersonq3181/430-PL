#lang racket 
(require rackunit)

;; 3 curried-add
(define curried-add (lambda (a) (lambda (b) (+ a b))))
(check-equal? ((curried-add 3) 4) 7)
(check-equal? ((curried-add 0) -1) -1)
(check-equal? ((curried-add 11) 12) 23)
(check-equal? ((curried-add 5) 3) 8)

;; 4 curry2
(define curry2 (lambda (f) (lambda (a) (lambda (b) (f a b)))))
(check-equal? (((curry2 /) 6) 2) 3)
(check-equal? (((curry2 +) 1) 10) 11)

;; 5 curry3
(define curry3 (lambda (f) (lambda (a) (lambda (b) (lambda (c) (f a b c))))))
(check-equal? ((((curry3 list) 1) 2) 3) '(1 2 3))
(define (test-func a b c)
  (+ a (+ b c)))
(check-equal? ((((curry3 test-func) 1) 2) 3) 6)

;; --- 6

;; contains (true if list contains symbol)
(define (contains? l s)
  (match l
    ['() #f]
    [(cons f r) (cond
                  [(equal? f s) #t]
                  [else (contains? r s)])]))
(check-equal? (contains? (list 'a 'b 'c 'd) 'e) #f)
(check-equal? (contains? (list 'a 'b 'c 'd) 'ab) #f)
(check-equal? (contains? (list 'a 'b 'c 'd) 'a) #t)
(check-equal? (contains? (list 'a 'b 'c 'd) 'b) #t)
(check-equal? (contains? (list 'a 'b 'c 'd) 'd) #t)

;; consumes source list of symbols, list of query symbols, and a list of booleans
;; indicating the corresponding elements in source which match symbols in the query
(define (in-list-many? source query) (map (lambda (s) (((curry2 contains?) query) s)) source))

(check-equal? (in-list-many? (list 'a 'b 'c) (list 'd 'e)) (list #f #f #f))
(check-equal? (in-list-many? (list 'a 'b 'c) (list 'a 'b 'e)) (list #t #t #f))
