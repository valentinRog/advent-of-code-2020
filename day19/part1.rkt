#lang racket

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define raw
  (string-remove (string-trim (port->string (current-input-port))) '("\r" "\"")))

(define rules
  (for/hash ((l (map (lambda (s) (string-split s ":"))
                     (string-split (first (string-split raw "\n\n")) "\n"))))
    (values (string->number (car l))
            (match (map string-trim (string-split (second l) "|"))
              ((list "a") "a")
              ((list "b") "b")
              (l (map (lambda (s) (map string->number (string-split s))) l))))))

(define messages (string-split (second (string-split raw "\n\n")) "\n"))

(define (apply-choices l)
  (let loop ((l l) (i 0))
    (let ((reduce-list
           (lambda (l ll) (for/fold ((acc (cons "" l))) ((i ll))
                            (let ((res (loop (cdr acc) i))) (cons (string-append (car acc) (car res)) (cdr res)))))))
      (if (empty? l) (cons "" l)
          (match (hash-ref rules i)
            ((list l1 l2) (match l
                            ((list 0 l ...) (reduce-list l l1))
                            ((list 1 l ...)(reduce-list l l2))))
            ((list l1) (reduce-list l l1))
            (c (cons c l)))))))

(define (match? s)
  (let loop ((acc '()))
    (let ((res (apply-choices acc)))
      (cond
        ((equal? (car res) s) #t)
        ((not (string-prefix? s (car res))) #f)
        ((not (empty? (cdr res))) #f)
        (#t (or (loop (append acc '(0))) (loop (append acc '(1)))))))))

(count match? messages)
