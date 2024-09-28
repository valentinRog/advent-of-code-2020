#lang racket

(define data
  (list->vector
   (map string->number (string-split (string-trim (port->string (current-input-port)))))))

(define range-size 25)
(define (ref i) (vector-ref data i))

(define (valid? index)
  (let ((range (in-range (- index range-size) index)))
    (for*/first
        ((i range)
         (ii range)
         #:when (= (+ (ref i) (ref ii)) (ref index)))
      #t)))

(define target
  (for/first ((i (in-naturals range-size)) #:when (not (valid? i))) (ref i)))

(let ((res
       (let loop ((i1 0))
         (define (find-i2 i2 acc)
           (cond
             ((= acc target) i2)
             ((= i2 (- (vector-length data) 1)) #f)
             (#t (find-i2 (add1 i2) (+ acc (ref (add1 i2)))))))
         (match (find-i2 (add1 i1) (+ (ref i1) (ref (add1 i1))))
           (#f (loop (add1 i1)))
           (i2
            (for/fold ((min-max (cons #f #f))) ((i (in-range i1 (add1 i2))))
              (define (extract n cmp)
                (match n
                  (#f (ref i))
                  (n #:when (cmp (ref i) n) (ref i))
                  (n n)))
              (cons (extract (car min-max) <) (extract (cdr min-max) >))))))))
  (+ (car res) (cdr res)))