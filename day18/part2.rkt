#lang racket

(define (tokenize s)
  (for/list ((token (string-split (string-replace (string-replace s "(" "( ") ")" " )"))))
    (match (string->number token) (#f token) (n n))))

(define data
  (map tokenize (string-split (string-trim (port->string (current-input-port))) "\n")))

(define (evaluate l)
  (define (safe-right? l)
    (match l
      ('() #t)
      ((list "+" _ ...) #f)
      ((list "(" _ ...) #t)
      ((list ")" _ ...) #t)
      ((list _ l ...) (safe-right? l))))
  (define (reduce l acc)
    (match l
      ((list n1 "*" n2 l ...) #:when (and (integer? n1) (integer? n2) (safe-right? l))
                              (append acc (list (* n1 n2)) l))
      ((list n1 "+" n2 l ...) #:when (and (integer? n1) (integer? n2))
                              (append acc (list (+ n1 n2)) l))
      ((list "(" n ")" l ...) (append acc (list n) l))
      ((list c l ...) (reduce l (append acc (list c))))))
  (match l ((list n) n) (_ (evaluate (reduce l '())))))

(for/sum ((l data)) (evaluate l))
