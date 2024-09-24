#lang racket

(define m (make-hash))

(struct point (x y) #:transparent)

(let ((lines
       (string-split
        (string-trim (port->string (current-input-port))))))
  (for ((line lines)
        (y (in-naturals)))
    (for ((c (string->list line))
          (x (in-naturals)))
      (hash-set! m (point x y) c))))

(define x-max
  (apply max (map (lambda (e) (point-x (car e))) (hash->list m))))

(let loop ((p (point 0 0))
           (n 0))
  (if (hash-has-key? m p)
      (loop
       (point (modulo (+ 3 (point-x p)) (add1 x-max)) (+ 1 (point-y p)))
       (if (eq? #\# (hash-ref m p)) (add1 n) n))
      n))