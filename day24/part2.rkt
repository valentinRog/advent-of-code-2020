#lang racket

(struct point (q r s) #:transparent)

(define (point-add p1 p2) (point (+ (point-q p1) (point-q p2))
                                 (+ (point-r p1) (point-r p2))
                                 (+ (point-s p1) (point-s p2))))

(define directions '("e" "se" "sw" "w" "nw" "ne"))

(define (neighbor p d)
  (match d
    ("e" (point-add p (point 1 0 -1)))
    ("se" (point-add p (point 0 1 -1)))
    ("sw" (point-add p (point -1 1 0)))
    ("w" (point-add p (point -1 0 1)))
    ("nw" (point-add p (point 0 -1 1)))
    ("ne" (point-add p (point 1 -1 0)))))

(define (neighbors p) (for/fold ((acc (set))) ((d directions)) (set-add acc (neighbor p d))))

(define (parse-instructions s)
  (let loop ((l '()) (s s))
    (if (equal? s "") l
        (let ((prefix (for/first ((prefix directions) #:when (string-prefix? s prefix)) prefix)))
          (loop (append l (list prefix)) (substring s (string-length prefix)))))))

(define data
  (map parse-instructions (map string-trim (string-split (port->string (current-input-port)) "\n"))))

(define (get-tile-p instructions)
  (for/fold ((p (point 0 0 0))) ((d instructions))
    (neighbor p d)))

(define state
  (for/fold ((acc (set))) ((p (map get-tile-p data)))
    (if (set-member? acc p) (set-remove acc p) (set-add acc p))))

(define (all-tiles state)
  (for/fold ((acc state)) ((p (set->list state))) (set-union acc (neighbors p))))

(define (next state)
  (for/fold ((acc (set))) ((p (all-tiles state)))
    (match (cons (set-member? state p) (set-count (set-intersect (neighbors p) state)))
      ((cons #t n) #:when (or (= n 0) (> n 2)) acc)
      ((cons #t _) (set-add acc p))
      ((cons #f 2) (set-add acc p))
      ((cons #f _) acc))))

(set-count (for/fold ((acc state)) ((_ (in-range 100))) (next acc)))
