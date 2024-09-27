#lang racket

(define (compute-group s) (length (apply set-intersect (map string->list (string-split s)))))

(apply +
       (map compute-group (string-split
                           (string-replace (string-trim (port->string (current-input-port))) "\r" "")
                           "\n\n")))