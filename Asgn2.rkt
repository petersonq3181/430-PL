#lang typed/racket
(require typed/rackunit)

;; ----- 2.1 Some Textbook Problems

; exercise 2.3.3 -- calculates total profit for a movie theater, based on num-attendees
(define (total-profit [n : Natural]) : Real
  (- (* n 4.5) 20))
(check-= (total-profit 2) -11 0.01)
(check-= (total-profit 5) 2.5 0.01)
(check-= (total-profit 30) 115 0.01)
(check-= (total-profit 100) 430 0.01)
(check-= (total-profit 100) 430.0 0.5)
(check-= (total-profit 50) 205.0 0.5)
(check-= (total-profit 250) 1105.0 0.5)

; exercise 3.3.3 -- calculates surface area of cylinder given radius and height
(define (area-cylinder [r : Real] [h : Real]) : Real
  (+ (* 2 (* 3.1415 (* r r))) (* h (* r (* 2 3.1415)))))
(check-= (area-cylinder 2 3) 62.83 0.001)
(check-= (area-cylinder 1 4) 31.415 0.001)
(check-= (area-cylinder 4 5) 226.194 0.5)
(check-= (area-cylinder 6 6) 452.389 0.5)



;; ----- 2.2 Magic Tricks
(define-type Magic-Trick (U Card-Trick Guillotine))
(struct Card-Trick ([decks : Natural] [volunteers : Natural]))
(struct Guillotine ([realism : Real] [has-tiger? : Boolean]))

; computes how long a trick will take (in mins), based on it's type
(define (trick-minutes [trick : Magic-Trick]) : Natural
  (match trick
    [(Card-Trick d v) (* d (expt 2 v))]
    [(Guillotine r hs?) (cond
                          [hs? 20]
                          [else 10])]))

(check-= (trick-minutes (Card-Trick 3 4)) 48 0.01)
(check-= (trick-minutes (Card-Trick 1 2)) 4 0.01)
(check-= (trick-minutes (Card-Trick 2 1)) 4 0.01)
(check-= (trick-minutes (Guillotine 1 #t)) 20 0.01)
(check-= (trick-minutes (Guillotine 10 #f)) 10 0.01)



;; ----- 2.3 Low-degree Polynomials
(define-type Polynomial (U Linear Quadratic))
(struct Linear ([A : Real] [B : Real])
  #:transparent)
(struct Quadratic ([A : Real] [B : Real] [C : Real])
  #:transparent)

; calculates given a polynomial and a variable value for x
(define (interp [p : Polynomial] [x : Real]) : Real
  (match p
    [(Linear a b) (+ (* a x) b)]
    [(Quadratic a b c) (+ (* a (* x x)) (+ (* b x) c))]))

(check-= (interp (Linear 3 5) 2) 11 0.01)
(check-= (interp (Linear 5 0) -3) -15 0.01)
(check-= (interp (Quadratic 1 2 3) 1) 6 0.01)
(check-= (interp (Quadratic 1 2 3) -2) 3 0.01)
(check-= (interp (Linear 2 3) 4) 11 0.5)
(check-= (interp (Quadratic 2 3 4) 5) 69 0.5)
(check-= (interp (Linear 4 5) 10) 45 0.5)
(check-= (interp (Quadratic 0 3 4) 5) 19 0.5)



;; ----- 2.4 Derivative
; accepts a polynomial and returns its derivative
(define (derivative [p : Polynomial]) : Polynomial
  (match p
    [(Linear a b) (Linear 0 a)]
    [(Quadratic a b c) (Linear (* a 2) b)]))
(check-equal? (derivative (Linear 4 3)) (Linear 0 4))
(check-equal? (derivative (Linear 1 5)) (Linear 0 1))
(check-equal? (derivative (Quadratic 3 7 4)) (Linear 6 7))
(check-equal? (derivative (Quadratic 1 1 1)) (Linear 2 1))



;; ----- 2.5 Binary Tree
(define-type BTree (U Node Leaf))
(struct Node ([s : Symbol] [l : BTree] [r : BTree])
  #:transparent)
(struct Leaf ()
  #:transparent)



;; ----- 2.6 Mirror
; mirrors a binary tree
(define (mirror (bt : BTree)) : BTree
  (match bt
    [(Leaf) bt]
    [(Node s l r) (Node s (mirror r) (mirror l))]))

(check-equal? (mirror (Node 'a (Leaf) (Leaf))) (Node 'a (Leaf) (Leaf)))
(check-equal? (mirror (Node 'a (Node 'b (Leaf) (Leaf)) (Node 'c (Leaf) (Leaf))))
              (Node 'a (Node 'c (Leaf) (Leaf)) (Node 'b (Leaf) (Leaf))))
(check-equal? (mirror (Node 'a (Node 'b (Leaf) (Leaf)) (Leaf))) (Node 'a (Leaf) (Node 'b (Leaf) (Leaf))))
(check-equal? (mirror (Node 'a (Leaf) (Node 'b (Leaf) (Leaf)))) (Node 'a (Node 'b (Leaf) (Leaf)) (Leaf)))
(check-equal? (mirror (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Leaf))) (Node 'a (Leaf) (Node 'b (Leaf) (Node 'c (Leaf) (Leaf)))))
(check-equal? (mirror (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Node 'ok (Node 'd (Leaf) (Leaf)) (Leaf))))
            (Node 'a (Node 'ok (Leaf) (Node 'd (Leaf) (Leaf))) (Node 'b (Leaf) (Node 'c (Leaf) (Leaf)))))



;; ----- 2.7 Left-Spine
; returns only the left-spine of a binary tree
(define (left-spine [bt : BTree]) : BTree
  (match bt
    [(Leaf) bt]
    [(Node s l r) (Node s (left-spine l) (Leaf))]))

(check-equal? (left-spine (Node 'a (Leaf) (Leaf))) (Node 'a (Leaf) (Leaf)))
(check-equal? (left-spine (Node 'a (Node 'b (Leaf) (Leaf)) (Node 'c (Leaf) (Leaf))))
              (Node 'a (Node 'b (Leaf) (Leaf)) (Leaf)))
(check-equal? (left-spine (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Node 'ok (Node 'd (Leaf) (Leaf)) (Leaf))))
              (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Leaf)))



;; ----- 2.8 Occurrences
; returns the number of occurrences of a symbol in a binary tree
(define (occurrences [bt : BTree] [x : Symbol]) : Natural
  (match bt
    [(Leaf) 0]
    [(Node s l r)
     (cond
       [(eq? x s) (+ 1 (occurrences l x) (occurrences r x))]
       [else (+ (occurrences l x) (occurrences r x))])]))

(check-equal? (occurrences (Node 'a (Leaf) (Leaf)) 'gg) 0)
(check-equal? (occurrences (Node 'a (Leaf) (Leaf)) 'a) 1)
(check-equal? (occurrences (Node 'a (Node 'a (Leaf) (Leaf)) (Node 'a (Leaf) (Leaf))) 'lol) 0)
(check-equal? (occurrences (Node 'a (Node 'a (Leaf) (Leaf)) (Node 'a (Leaf) (Leaf))) 'a) 3)
(check-equal? (occurrences (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Node 'ok (Node 'd (Leaf) (Leaf)) (Leaf))) 'gg) 0)
(check-equal? (occurrences (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Node 'ok (Node 'd (Leaf) (Leaf)) (Leaf))) 'a) 1)
(check-equal? (occurrences (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Node 'ok (Node 'a (Leaf) (Leaf)) (Leaf))) 'a) 2)
(check-equal? (occurrences (Node 'a (Node 'b (Node 'c (Leaf) (Leaf)) (Leaf)) (Node 'ok (Node 'a (Leaf) (Leaf)) (Leaf))) 'ok) 1)
(check-equal? (occurrences (Node 'a (Node 'b (Node 'a (Leaf) (Leaf)) (Leaf)) (Node 'a (Node 'a (Leaf) (Leaf)) (Leaf))) 'a) 4)



;; ----- 2.9 Substitution
; substitutes a new binary tree at the occurance of a symbol in a given binary tree
(define (subst [bt-src : BTree] [x : Symbol] [bt-rep : BTree]) : BTree
  (match bt-src
    [(Leaf) bt-src]
    [(Node s l r) (cond
                    [(eq? x s) bt-rep]
                    [else (Node s (subst l x bt-rep) (subst r x bt-rep))])]))

(check-equal? (subst (Node 'a (Leaf) (Leaf)) 'a (Node 'gg (Node 'a (Leaf) (Leaf)) (Leaf)))
              (Node 'gg (Node 'a (Leaf) (Leaf)) (Leaf)))
(check-equal? (subst (Node 'a (Leaf) (Leaf)) 'ok (Node 'gg (Node 'a (Leaf) (Leaf)) (Leaf)))
              (Node 'a (Leaf) (Leaf)))
(check-equal? (subst (Node 'a (Node 'b (Node 'a (Leaf) (Leaf)) (Leaf)) (Node 'a (Node 'a (Leaf) (Leaf)) (Leaf))) 'b (Node 'gg (Node 'ok (Leaf) (Leaf)) (Leaf)))
              (Node 'a (Node 'gg (Node 'ok (Leaf) (Leaf)) (Leaf)) (Node 'a (Node 'a (Leaf) (Leaf)) (Leaf))))
(check-equal? (subst (Node 'a (Node 'b (Node 'a (Leaf) (Leaf)) (Leaf)) (Node 'a (Node 'a (Leaf) (Leaf)) (Leaf))) 'a (Node 'gg (Node 'ok (Leaf) (Leaf)) (Leaf)))
              (Node 'gg (Node 'ok (Leaf) (Leaf)) (Leaf)))
(check-equal? (subst (Node 'a (Node 'b (Node 'a (Leaf) (Leaf)) (Leaf)) (Node 'a (Node 'a (Leaf) (Leaf)) (Leaf))) 'y (Node 'gg (Node 'ok (Leaf) (Leaf)) (Leaf)))
              (Node 'a (Node 'b (Node 'a (Leaf) (Leaf)) (Leaf)) (Node 'a (Node 'a (Leaf) (Leaf)) (Leaf))))
(check-equal? (subst (Node 'a (Node 'b (Leaf) (Leaf)) (Node 'c (Leaf) (Node 'd (Leaf) (Leaf)))) 'd (Node 'gg (Node 'ggg (Leaf) (Leaf)) (Leaf)))
              (Node 'a (Node 'b (Leaf) (Leaf)) (Node 'c (Leaf) (Node 'gg (Node 'ggg (Leaf) (Leaf)) (Leaf)))))
