#lang racket

(struct point (x y z w) #:transparent)

(define (around p)
  (define (range n) (in-range (- n 1) (+ n 2)))
  (for*/fold ((acc (set)))
             ((x (range (point-x p)))
              (y (range (point-y p)))
              (z (range (point-z p)))
              (w (range (point-w p))))
    (let ((np (point x y z w))) (if (equal? np p) acc (set-add acc np)))))

(define data
  (for/fold ((acc (set)))
            ((line (string-split (string-trim (port->string (current-input-port))) "\n"))
             (y (in-naturals)))
    (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
      (match c
        (#\# (set-add acc (point x y 0 0)))
        (_ acc)))))

(define (next-state active)
  (let ((res (for/fold ((acc (set))) ((p (set->list active)))
               (match (set-count (set-intersect (around p) active))
                 ((or 2 3) (set-add acc p))
                 (_ acc))))
        (inactive (set-subtract (for/fold ((acc (set))) ((p (set->list active)))
                                  (set-union acc (around p))) active)))
    (for/fold ((acc res)) ((p (set->list inactive)))
      (match (set-count (set-intersect (around p) active))
        (3 (set-add acc p))
        (_ acc)))))

(set-count (for/fold ((acc data)) ((_ (in-range 6))) (next-state acc)))
