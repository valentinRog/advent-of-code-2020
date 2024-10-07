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

(define (apply-choices l i)
  (let loop ((l l) (i i))
    (let ((reduce-list
           (lambda (l ll) (for/fold ((acc (cons "" l))) ((i ll))
                            (let ((res (loop (cdr acc) i)))
                              (cons (string-append (car acc) (car res)) (cdr res)))))))
      (if (empty? l) (cons "" l)
          (match (hash-ref rules i)
            ((list l1 l2) (match l
                            ((list 0 l ...) (reduce-list l l1))
                            ((list 1 l ...) (reduce-list l l2))))
            ((list l1) (reduce-list l l1))
            (c (cons c l)))))))

(define (generate i)
  (let loop ((acc '()))
    (let ((res (apply-choices acc i)))
      (cond ((not (empty? (cdr res))) (set (car res)))
            (#t (set-union (loop (append acc '(0))) (loop (append acc '(1)))))))))

(define r42 (generate 42))
(define r31 (generate 31))

(define (chunk s)
  (let ((len (string-length (set-first r42))))
    (let ((ss (substring s 0 len)))
      (if (<= (string-length s) len) (list ss) (append (list ss) (chunk (substring s len)))))))

(define (match? s)
  (let loop ((l (chunk s)) (n42 0) (n31 0))
    (match l
      ('() (and (> n42 n31) (> n31 0)))
      ((list s l ...) (match (list (= n31 0) (set-member? r42 s) (set-member? r31 s))
                        ((list _ _ #t) (loop l n42 (add1 n31)))
                        ((list #f #t _) #f)
                        ((list #t #t _) (loop l (add1 n42) n31)))))))

(count match? messages)
