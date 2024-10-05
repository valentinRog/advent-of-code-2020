#lang racket

(define (in-range? r n) (and (>= n (car r)) (<= n (cdr r))))

(define raw (string-replace (string-trim (port->string (current-input-port))) "\r" ""))

(define ranges
  (for/list
      ((s (filter (lambda (s) (string-contains? s "-"))
                  (string-split (car (string-split raw "\n\n"))))))
    (let ((l (map string->number (string-split s "-")))) (cons (car l) (second l)))))

(define numbers
  (map string->number
       (string-split
        (string-join (drop (string-split (last (string-split raw "\n\n")) "\n") 1) ",")
        ",")))

(for/sum ((n numbers)) (if (ormap (lambda (r) (in-range? r n)) ranges) 0 n))