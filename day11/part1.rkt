#lang racket

(struct point (x y) #:transparent)

(define (point-add p1 p2) (point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))

(define data
  (let ((lines (string-split (string-trim (port->string (current-input-port))))))
    (for/fold ((acc (set))) ((line lines) (y (in-naturals)))
      (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
        (if (equal? c #\L) (set-add acc (point x y)) acc)))))

(define (count-around p state)
  (count (lambda (d) (set-member? state (point-add p d)))
         (list
          (point 0 -1)
          (point 1 -1)
          (point 1 0)
          (point 1 1)
          (point 0 1)
          (point -1 1)
          (point -1 0)
          (point -1 -1))))

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
