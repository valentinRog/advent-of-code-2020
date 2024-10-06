#lang racket

(define (tokenize s)
  (for/list ((token (string-split (string-replace (string-replace s "(" "( ") ")" " )"))))
    (match (string->number token) (#f token) (n n))))

(define data
  (map tokenize (string-split (string-trim (port->string (current-input-port))) "\n")))

(define (evaluate l)
  (define (reduce l acc)
    (match l
      ((list n1 c n2 l ...) #:when (and (integer? n1) (integer? n2))
                            (append acc (list ((match c ("+" +) ("*" *)) n1 n2)) l))
      ((list "(" n ")" l ...) (append acc (list n) l))
      ((list c l ...) (reduce l (append acc (list c))))))
  (match l ((list n) n) (_ (evaluate (reduce l '())))))

(for/sum ((l data)) (evaluate l))
