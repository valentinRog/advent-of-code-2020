#lang racket

(define data
  (let ((lines (string-split (string-trim (port->string (current-input-port))))))
    (for/fold ((acc (set))) ((line lines) (y (in-naturals)))
      (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
        (if (equal? c #\L) (set-add acc (make-rectangular x y)) acc)))))

(define (max n1 n2) (if (> n1 n2) n1 n2))

(define max-p
  (for/fold ((acc 0)) ((p data))
    (let ((x (max (real-part acc) (real-part p)))
          (y (max (imag-part acc) (imag-part p))))
      (make-rectangular x y))))

(define (outside? p)
  (or
   (< (real-part p) 0)
   (< (imag-part p) 0)
   (> (magnitude p) (magnitude max-p))))

(define (count-around p state)
  (define (check-dir d)
    (let loop ((p (+ p d)))
      (cond ((set-member? state p) #t)
            ((set-member? data p) #f)
            ((outside? p) #f)
            (#t (loop (+ p d))))))
  (count check-dir (list -i 1-i 1 1+i +i -1+i -1 -1-i)))

(define (next-state state)
  (for/fold ((acc (set))) ((p data))
    (match (cons (set-member? state p) (count-around p state))
      ((cons #t n) #:when (>= n 5) acc)
      ((cons #t _) (set-add acc p))
      ((cons #f 0) (set-add acc p))
      ((cons #f _) acc))))

(let loop ((cache (set)) (state (set)))
  (if (set-member? cache state) (set-count state)
      (loop (set-add cache state) (next-state state))))
