#lang racket

(define data
  (let ((raw (string-split (string-trim (port->string (current-input-port))))))
    (for/fold ((acc (list)))
              ((e (map string->number (string-split (second raw) ",")))
               (i (in-naturals)))
      (if (not e) acc (cons (cons e i) acc)))))

(define (compatible-buses b)
  (map car (filter (lambda (e) (= (abs (- (cdr b) (cdr e))) (car e))) data)))

(define best-bus
  (for/fold ((acc (car data))) ((e (cdr data)))
    (if (> (length (compatible-buses e)) (length (compatible-buses acc))) e acc)))

(define (valid? n) (andmap (lambda (e) (= (remainder (+ n (cdr e)) (car e)) 0)) data))

(let ((step (* (car best-bus) (apply * (compatible-buses best-bus)))))
  (let loop ((acc step))
    (match (- acc (cdr best-bus))
      (n #:when (valid? n) n)
      (_ (loop (+ acc step))))))
