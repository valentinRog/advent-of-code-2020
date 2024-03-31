#lang racket

(struct pass (i1 i2 c s) #:transparent)

(define (parse s)
  (let*
      ((a (string-split s ": "))
       (b (string-split (car a)))
       (r (map string->number (string-split (car b) "-")))
       (c (string-ref (cadr b) 0)))
    (pass (car r) (cadr r) c (cadr a))))


(define data
  (map parse
       (string-split
        (string-trim
         (port->string (current-input-port))) "\n")))

(define (valid? p)
  (let* ((i1 (pass-i1 p))
         (i2 (pass-i2 p))
         (c (pass-c p))
         (s (pass-s p)))
    (xor (equal? (string-ref s (sub1 i1)) c)
         (equal? (string-ref s (sub1 i2)) c))))

(for/fold ((n 0))
          ((p data))
  (if (valid? p) (add1 n) n))
