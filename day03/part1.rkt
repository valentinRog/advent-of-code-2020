#lang racket

(define m (make-hash))

(let ((lines
       (string-split
        (string-trim (port->string (current-input-port))))))
  (for ((line lines)
        (y (in-naturals)))
    (for ((c (string->list line))
          (x (in-naturals)))
      (hash-set! m (make-rectangular x y) c))))

(define x-max
  (apply max (map (lambda (e) (real-part (car e))) (hash->list m))))

(let loop ((p 0)
           (n 0))
  (if (hash-has-key? m p)
      (loop
       (make-rectangular (modulo (+ 3 (real-part p)) (add1 x-max)) (+ 1 (imag-part p)))
       (if (eq? #\# (hash-ref m p)) (add1 n) n))
      n))