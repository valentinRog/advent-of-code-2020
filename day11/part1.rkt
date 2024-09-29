#lang racket

(define data
  (let ((lines (string-split (string-trim (port->string (current-input-port))))))
    (for/fold ((acc (set))) ((line lines) (y (in-naturals)))
      (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
        (if (equal? c #\L) (set-add acc (make-rectangular x y)) acc)))))

(define (count-around p state)
  (count (lambda (d) (set-member? state (+ p d)))
         (list -i 1-i 1 1+i +i -1+i -1 -1-i)))

(define (next-state state)
  (for/fold ((acc (set))) ((p data))
    (match (cons (set-member? state p) (count-around p state))
      ((cons #t n) #:when (>= n 4) acc)
      ((cons #t _) (set-add acc p))
      ((cons #f 0) (set-add acc p))
      ((cons #f _) acc))))

(let loop ((cache (set)) (state (set)))
  (if (set-member? cache state) (set-count state)
      (loop (set-add cache state) (next-state state))))
