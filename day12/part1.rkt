#lang racket

(define data
  (let ((data (string-split (string-trim (port->string (current-input-port))))))
    (map (lambda (e) (cons (string-ref e 0) (string->number (substring e 1)))) data)))

(let ((p
       (car
        (for/fold ((acc (cons 0 1))) ((ins data))
          (let ((p (car acc)) (d (cdr acc)))
            (match ins
              ((cons #\N n) (cons (+ p (* n +i)) d))
              ((cons #\S n) (cons (+ p (* n -i)) d))
              ((cons #\E n) (cons (+ p n) d))
              ((cons #\W n) (cons (+ p (* n -1)) d))
              ((cons #\L n) (cons p (* d (expt +i (quotient n 90)))))
              ((cons #\R n) (cons p (* d (expt -i (quotient n 90)))))
              ((cons #\F n) (cons (+ p (* n d)) d))))))))
  (+ (abs (real-part p)) (abs (imag-part p))))
