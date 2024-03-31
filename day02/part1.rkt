#lang racket

(struct pass (range c s) #:transparent)

(define (parse s)
  (let*
      ((a (string-split s ": "))
       (b (string-split (car a)))
       (r (map string->number (string-split (car b) "-")))
       (c (string-ref (cadr b) 0)))
    (pass r c (cadr a))))


(define data
  (map parse
       (string-split
        (string-trim
         (port->string (current-input-port))) "\n")))

(define (count s c)
  (for/fold ((n 0))
            ((cc s))
    (if (equal? cc c) (add1 n) n)))

(define (valid? p)
  (let* ((n0 (car (pass-range p)))
         (n1 (cadr (pass-range p)))
         (c (pass-c p))
         (s (pass-s p))
         (n (count s c)))
    (and (>= n n0) (<= n n1))))

(for/fold ((n 0))
          ((p data))
  (if (valid? p) (add1 n) n))
