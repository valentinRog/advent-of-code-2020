#lang racket

(define data
  (map string->number (map string (string->list (string-trim (port->string (current-input-port)))))))

(define (rotate l) (append (cdr l) (list (car l))))

(define (rotate-to l c) (if (= (car l) c) l (rotate-to (rotate l) c)))

(define (list-max l)
  (for/fold ((acc #f)) ((e l))
    (match acc
      (#f e)
      (acc #:when (< acc e) e)
      (_ acc))))

(define (insert-second l ll) (cons (car l) (append ll (cdr l))))

(define (move l)
  (define (find-target l c i)
    (match (cons (member (- c i) l) (- c i))
      ((cons #f 0) (list-max l))
      ((cons #f _) (find-target l c (add1 i)))
      ((cons _ _)  (- c i))))
  (let* ((tmp (take (cdr l) 3)) (c (car l)) (l (cons c (drop (cdr l) 3))))
    (rotate (rotate-to (insert-second (rotate-to l (find-target l c 1)) tmp) c))))

(displayln
 (string-join (map number->string
                   (drop (rotate-to (for/fold ((l data)) ((_ (in-range 100))) (move l)) 1) 1))
              ""))
