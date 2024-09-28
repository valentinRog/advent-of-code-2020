#lang racket

(struct point (x y) #:transparent)

(define data
  (let ((lines (string-split (string-trim (port->string (current-input-port))))))
    (for/fold ((acc (set))) ((line lines) (y (in-naturals)))
      (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
        (if (equal? c #\L) (set-add acc (point x y)) acc)))))

(define (around p)
  (let ((x (point-x p)) (y (point-y p)))
    (set
     (point x (- y 1))
     (point (+ x 1) (- y 1))
     (point (+ x 1) y)
     (point (+ x 1) (+ y 1))
     (point x (+ y 1))
     (point (- x 1) (+ y 1))
     (point (- x 1) y)
     (point (- x 1) (- y 1)))))

(define (next-state state)
  (for/fold ((acc (set))) ((p data))
    (if (set-member? state p)
        (if (>= (set-count (set-intersect state (around p))) 4) acc (set-add acc p))
        (if (set-empty? (set-intersect state (around p))) (set-add acc p) acc))))

(let loop ((cache (set)) (state (set)))
  (if (set-member? cache state) (set-count state)
      (loop (set-add cache state) (next-state state))))
