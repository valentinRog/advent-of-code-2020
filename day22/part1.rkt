#lang racket

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define data
  (let (( data (string-remove (string-trim (port->string (current-input-port))) '("\r"))))
    (map (lambda (s) (map string->number (drop (string-split s "\n") 1)))
         (string-split data "\n\n"))))

(define (fight l1 l2)
  (match (cons l1 l2)
    ((cons '() l2) l2)
    ((cons l1 '()) l1)
    ((cons (list c1 l1 ...) (list c2 l2 ...))
     (if (> c1 c2) (fight (append l1 (list c1 c2)) l2) (fight l1 (append l2 (list c2 c1)))))))

(for/sum ((c (reverse (fight (car data) (second data)))) (i (in-naturals 1))) 
    (* c i))
