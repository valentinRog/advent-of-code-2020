#lang racket

(define (bin-search i1 i2 l)
  (if (= i1 i2) i1 (let ((mid (+ i1 (quotient (- i2 i1) 2))))
                     (match (car l)
                       ((or #\F #\L) (bin-search i1 mid (cdr l)))
                       ((or #\B #\R) (bin-search (add1 mid) i2 (cdr l)))))))

(define data (string-split (string-trim (port->string (current-input-port)))))

(define (id s) (+ (* 8 (bin-search 0 127 (string->list (substring s 0 7))))
                  (bin-search 0 7 (string->list (substring s 7)))))

(let loop ((l (sort (map id data) <)))
  (let ((target (add1 (first l)))) (if (equal? (second l) target) (loop (cdr l)) target)))
