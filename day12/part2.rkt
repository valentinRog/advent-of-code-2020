#lang racket

(define data
  (let ((data (string-split (string-trim (port->string (current-input-port))))))
    (map (lambda (e) (cons (string-ref e 0) (string->number (substring e 1)))) data)))

(let ((p
       (car
        (for/fold ((acc (cons 0 10+1i))) ((ins data))
          (let ((p (car acc)) (wp (cdr acc)))
            (match ins
              ((cons #\N n) (cons p (+ wp (* n +i))))
              ((cons #\S n) (cons p (+ wp (* n -i))))
              ((cons #\E n) (cons p (+ wp n)))
              ((cons #\W n) (cons p (+ wp (* n -1))))
              ((cons #\L n) (cons p (* wp (expt +i (quotient n 90)))))
              ((cons #\R n) (cons p (* wp (expt -i (quotient n 90)))))
              ((cons #\F n) (cons (+ p (* n wp)) wp))))))))
  (+ (abs (real-part p)) (abs (imag-part p))))
