#lang racket

(define data
  (filter-map string->number
              (map string-trim
                   (port->lines (current-input-port)))))

(for*/first ((n1 data) (n2 data) #:when (= 2020 (+ n1 n2)))
  (* n1 n2))