#lang racket

(define (in-range? r n) (and (>= n (car r)) (<= n (cdr r))))

(define (in-field? field n) (or (in-range? (car field) n) (in-range? (cdr field) n)))

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define raw (string-remove (string-trim (port->string (current-input-port))) '("\r")))

(define (list->pair l) (cons (car l) (second l)))

(define (parse-ticket s) (list->vector (map string->number (string-split s ","))))

(define fields
  (for/vector ((line (string-split (car (string-split raw "\n\n")) "\n")))
    (list->pair
     (map (lambda (s) (list->pair (map string->number (string-split s "-"))))
          (filter (lambda (s) (string-contains? s "-")) (string-split line))))))

(define (compatibles-fields n)
  (for/fold ((acc (set)))
            ((i (in-range (vector-length fields))))
    (if (in-field? (vector-ref fields i) n) (set-add acc i) acc)))

(define ticket (parse-ticket (second (string-split (second (string-split raw "\n\n")) "\n"))))
(define tickets
  (filter
   (lambda (l)
     (andmap (lambda (n) (ormap (lambda (field) (in-field? field n)) (vector->list fields)))
             (vector->list l)))
   (map parse-ticket (drop (string-split (last (string-split raw "\n\n")) "\n") 1))))

(define (reduce-intersections l)
  (map (lambda (e)
         (for/fold ((acc e)) ((hs l))
           (if (and (subset? hs acc) (not (equal? acc hs))) (set-subtract acc hs) acc)))
       l))

(define intersections
  (for/list ((i (range (vector-length fields))))
    (for/fold ((acc (for/set ((i (in-range (vector-length fields)))) i))) ((v tickets))
      (set-intersect acc (compatibles-fields (vector-ref v i))))))

(define permutations
  (let loop ((prev intersections))
    (if (andmap (lambda (e) (= (set-count e) 1)) prev)
        (list->vector (set->list (map set-first prev)))
        (loop (reduce-intersections prev)))))

(for/product ((i (in-range 6)))
  (vector-ref ticket (vector-member i permutations)))
