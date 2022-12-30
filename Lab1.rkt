#lang typed/racket
(require typed/rackunit)



; ex. 15: is true if its not sunny or its friday 
(define (sun-fri [sunny : Boolean] [friday : Boolean]) : Boolean
  (or (not sunny) friday))

(check-equal? (sun-fri #t #t) #t)
(check-equal? (sun-fri #t #f) #f)
(check-equal? (sun-fri #f #t) #t)
(check-equal? (sun-fri #f #f) #t)



; ex. 19: string insert a "_" at position i
(define (string-insert [str : String] [i : Natural]) : String
  (string-append (substring str 0 i) "_" (substring str i)))

(check-equal? (string-insert "ok" 1) "o_k")
(check-equal? (string-insert "abcd" 3) "abc_d")
(check-equal? (string-insert "abcd" 4) "abcd_")



; ----- ex. 27
(define NUM-PPL : Real 120) 
(define PRICE : Real 5.0)
(define AVG-PPL-CHG : Real 15)
(define AVG-PRC-CHG : Real 0.1)
(define COST : Real 180)
(define COST-PER : Real 0.04)

; calculates attendees from ticket-price 
(define (attendees [ticket-price : Real]) : Real
  (- NUM-PPL (* (- ticket-price PRICE) (/ AVG-PPL-CHG AVG-PRC-CHG))))

; calculates revenue from ticket-price and num attendees 
(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

; calculates cost from ticket-price and num attendees 
(define (cost [ticket-price : Real]) : Real
  (+ COST (* COST-PER (attendees ticket-price))))

; calculates profit from revenue and cost 
(define (profit [ticket-price : Real]) : Real
  (- (revenue ticket-price)
     (cost ticket-price)))

(check-equal? (profit 10) -6454.8)
(check-equal? (profit 5) 415.2)
(check-equal? (profit 3) 1063.2)


; calculates interest made on a deposit in one year 
(define (interest [deposit : Real]) : Real
  (cond
    [(<= deposit 1000) (* deposit 0.04)]
    [(<= deposit 5000) (* deposit 0.045)]
    [else (* deposit 0.05)]
    )
  )

(check-= (interest 1000) 40.0 0.01)
(check-= (interest 500) 20.0 0.01)
(check-= (interest 3000) 135.0 0.01)
(check-= (interest 10000) 500.0 0.01)



; ----- 4.4 Structures
(define-type Office-Furniture (U Desk Bookshelves))

(struct Desk (
              [width : Real]
              [height : Real]
              [depth : Real]
              )
  #:transparent
  )

(struct Bookshelves (
                     [depth : Real]
                     [n-shelves : Real]
                     [width : Real]
                     )
  #:transparent
  )

; calculates how much floor space a Desk takes up 
(define (furniture-footprint [o : Office-Furniture]) : Real
  (match o
      [(Desk w h d) (* w d)]
      [(Bookshelves d n w) (* w d)]
      )
  )
(check-= (furniture-footprint (Desk 10 10 10)) 100 0.01)
(check-= (furniture-footprint (Desk 3 4 5)) 15 0.01)
(check-= (furniture-footprint (Desk 1 2 10)) 10 0.01)
(check-= (furniture-footprint (Bookshelves 10 10 10)) 100 0.01)
(check-= (furniture-footprint (Bookshelves 3 4 5)) 15 0.01)
(check-= (furniture-footprint (Bookshelves 1 2 10)) 10 0.01) 
