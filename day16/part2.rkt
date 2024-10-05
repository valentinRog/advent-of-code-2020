#lang racket

(define (in-range? r n) (and (>= n (car r)) (<= n (cdr r))))

(define (in-field? field n) (or (in-range? (car field) n) (in-range? (cdr field) n)))

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define raw (string-remove (string-trim (port->string (current-input-port))) '("\r")))

(define (list->pair l) (cons (car l) (second l)))

(define (parse-ticket s) (map string->number (string-split s ",")))

(define fields
  (for/list ((line (string-split (car (string-split raw "\n\n")) "\n")))
    (list->pair
     (map (lambda (s) (list->pair (map string->number (string-split s "-"))))
          (filter (lambda (s) (string-contains? s "-")) (string-split line))))))

(define ticket (parse-ticket (second (string-split (second (string-split raw "\n\n")) "\n"))))
(define tickets
  (filter
   (lambda (l) (andmap (lambda (n) (ormap (lambda (field) (in-field? field n)) fields)) l))
   (map parse-ticket (drop (string-split (last (string-split raw "\n\n")) "\n") 1))))

(define (valid-arrangement? v)
  (define (valid-ticket? ticket-v i)
    (cond
      ((= i (vector-length v)) #t)
      ((in-field? (vector-ref (list->vector fields) (vector-ref v i)) (vector-ref ticket-v i)) (valid-ticket? ticket-v (add1 i)))
      (#t #f)))
  (andmap (lambda (l) (valid-ticket? (list->vector l) 0)) tickets))

(define len (length fields))
(define (find-arrangement acc)
    (displayln acc)
  (if (and (>= (vector-length acc) 5) (> (vector-ref acc 3) 4)) (displayln acc) 7)
  (cond
    ((not (valid-arrangement? acc)) #f)
    ((= (vector-length acc) len) acc)
    (#t (let loop ((i 0))
          (cond
            ((= i len) #f)
            ((not (vector-member i acc))
             (match (find-arrangement (vector-append acc (vector i)))
               (#f (loop (add1 i)))
               (acc acc)))
            (#t (loop (add1 i))))))))

(for/product
    ((i (take (vector->list (find-arrangement (vector))) 6)))
  (vector-ref (list->vector ticket) i))
