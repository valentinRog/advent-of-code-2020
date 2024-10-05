#lang racket

(define data
  (map string->number
       (string-split (string-trim (port->string (current-input-port))) ",")))

(define m (for/hash ((n data) (i (in-range (- (length data) 1)))) (values n i)))

(let loop ((prev (last data)) (m m) (i (- (length data) 1)))
  (cond ((= i (- 30000000 1)) prev)
        ((hash-has-key? m prev) (loop (- i (hash-ref m prev)) (hash-set m prev i) (add1 i)))
        (#t (loop 0 (hash-set m prev i) (add1 i)))))