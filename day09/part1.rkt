#lang racket

(define data
  (list->vector
   (map string->number (string-split (string-trim (port->string (current-input-port)))))))

(define range-size 25)
(define (ref i) (vector-ref data i))

(define (valid? index)
  (let ((range (in-range (- index range-size) index)))
    (for*/first
        ((i range)
         (ii range)
         #:when (= (+ (ref i) (ref ii)) (ref index)))
      #t)))

(for/first ((i (in-naturals range-size)) #:when (not (valid? i))) (ref i))