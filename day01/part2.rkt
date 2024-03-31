#lang racket

(define data
  (filter-map string->number
              (map string-trim
                   (port->lines (current-input-port)))))

(for*/first ((n1 data) (n2 data) (n3 data) #:when (= 2020 (apply + (list n1 n2 n3))))
  (apply * (list n1 n2 n3)))