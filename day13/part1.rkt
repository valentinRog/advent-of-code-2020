#lang racket

(define raw (string-split (string-trim (port->string (current-input-port)))))
(define depart (string->number (car raw)))
(define buses (filter values (map string->number (string-split (second raw) ","))))

(let ((res
       (for/fold ((acc #f)) ((id buses))
         (let ((dt (- id (remainder depart id))))
           (match acc
             (#f (cons id dt))
             ((cons _ acc-dt) #:when (< dt acc-dt) (cons id dt))
             (acc acc))))))
  (* (car res) (cdr res)))
