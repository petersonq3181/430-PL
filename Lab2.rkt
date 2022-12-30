#lang typed/racket
(require typed/rackunit) 

(define (show-example b)
  (begin
    (printf "sum of magnitude of elements: ~s\n"
            (array-axis-fold (array-map magnitude (my-fft b)) 0 + 0))
    (plot (points (in-array (array->plottable (my-fft b))))
          #:y-max 1500
          #:x-max 1024
          #:height 300)))

(printf "magnitude of sum of elements: ~s\n"
            (array-map magnitude (array-axis-fold (my-fft b) 0 + 0)))


(define (srl:map-append func lst)
  (cond [(null? lst)]
      [lst]
      [else (append (func (car lst))
              (srl:map-append func (cdr lst)))]))



