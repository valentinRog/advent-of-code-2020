#lang racket

(define data
  (let ((l (sort
            (map string->number (string-split (string-trim (port->string (current-input-port)))))
            > )))
    (cons 0 (reverse (cons (+ 3 (car l)) l)))))

(define (sliding-window v l)
  (for/list ((i (in-range (- l 1) (vector-length v))))
    (for/list ((i (in-range (- (add1 i) l) (add1 i)))) (vector-ref v i))))

(let
    ((res
      (for/fold ((acc (cons 0 0))) ((e (sliding-window (list->vector data) 2)))
        (match (- (second e) (car e))
          (1 (cons (add1 (car acc)) (cdr acc)))
          (3 (cons (car acc) (add1 (cdr acc))))))))
  (* (car res) (cdr res)))
