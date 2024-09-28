#lang racket

(define data
  (reverse
   (let ((l (sort
             (map string->number (string-split (string-trim (port->string (current-input-port)))))
             > )))
     (cons (+ 3 (car l)) l))))

(define chunked-data
  (reverse
   (map reverse
        (for/fold ((acc (list (list 0)))) ((j data))
          (match (- j (caar acc))
            (3 (cons (list j) acc))
            (_ (cons (cons j (car acc)) (cdr acc))))))))

(define (sliding-window v l)
  (for/list ((i (in-range (- l 1) (vector-length v))))
    (for/list ((i (in-range (- (add1 i) l) (add1 i)))) (vector-ref v i))))

(define (valid? v) (andmap (lambda (e) (<= (- (second e) (first e)) 3)) (sliding-window v 2)))

(define (erase/copy v i) (vector-filter-not (lambda (e) (= e (vector-ref v i))) v))

(define (arrangements v acc)
  (if (or (set-member? acc v) (not (valid? v))) acc
      (let ((acc (set-add acc v)))
        (for/fold ((acc acc)) ((i (in-range 1 (- (vector-length v) 1))))
          (set-union acc (arrangements (erase/copy v i) acc))))))

(for/product ((l chunked-data)) (set-count (arrangements (list->vector l) (set))))
