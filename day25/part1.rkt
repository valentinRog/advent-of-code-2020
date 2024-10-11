#lang racket

(define data
  (map string->number
       (map string-trim (string-split (string-trim (port->string (current-input-port))) "\n"))))

(define card-public-key (car data))
(define door-public-key (second data))

(define (compute-loop-size public-key)
  (let loop ((i 0) (n 1)) (if (= n public-key) i (loop (add1 i) (remainder (* n 7) 20201227)))))

(for/fold ((acc 1)) ((_ (in-range (compute-loop-size card-public-key))))
  (remainder (* acc door-public-key) 20201227))

