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

(define (compute right down)
  (let loop ((p 0)
             (n 0))
    (if (hash-has-key? m p)
        (loop
         (make-rectangular (modulo (+ right (real-part p)) (add1 x-max)) (+ down (imag-part p)))
         (if (eq? #\# (hash-ref m p)) (add1 n) n))
        n)))

(apply *
       (map (lambda (e) (compute (car e) (cadr e)))
            (list '(1 1) '(3 1) '(5 1) '(7 1) '(1 2))))