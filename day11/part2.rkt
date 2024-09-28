#lang racket

(struct point (x y) #:transparent)

(define (point-add p1 p2) (point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))

(define data
  (let ((lines (string-split (string-trim (port->string (current-input-port))))))
    (for/fold ((acc (set))) ((line lines) (y (in-naturals)))
      (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
        (if (equal? c #\L) (set-add acc (point x y)) acc)))))

(define (max n1 n2) (if (> n1 n2) n1 n2))

(define max-p
  (for/fold ((acc (point 0 0))) ((p data))
    (let ((x (max (point-x acc) (point-x p))) (y (max (point-y acc) (point-y p)))) (point x y))))

(define (outside? p)
  (or
   (< (point-x p) 0)
   (< (point-y p) 0)
   (> (point-x p) (point-x max-p))
   (> (point-y p) (point-y max-p))))

(define (count-around p state)
  (define (check-dir d)
    (let loop ((p (point-add p d)))
      (cond ((set-member? state p) #t)
            ((set-member? data p) #f)
            ((outside? p) #f)
            (#t (loop (point-add p d))))))
  (count check-dir (list
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
    (if (set-member? state p)
        (if (>= (count-around p state) 5) acc (set-add acc p))
        (if (= (count-around p state) 0) (set-add acc p) acc))))

(let loop ((cache (set)) (state (set)))
  (if (set-member? cache state) (set-count state)
      (loop (set-add cache state) (next-state state))))
