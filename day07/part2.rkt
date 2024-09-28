#lang racket

(define (parse-bag line)
  (let* ((l (string-split line))
         (name (string-join (take l 2))))
    (define (extract-bags l bags)
      (cond
        ((null? l) bags)
        (else (match (string->number (car l))
                (#f (extract-bags (cdr l) bags))
                (n (extract-bags
                    (cdr l)
                    (hash-set bags (string-join (take (cdr l) 2)) n)))))))
    (list name (extract-bags l (make-immutable-hash)))))

(define target "shiny gold")

(define data
  (for/fold ((m (make-immutable-hash)))
            ((bag
              (map parse-bag (string-split
                              (string-replace
                               (string-replace
                                (string-trim (port->string (current-input-port)))
                                "." "")
                               "," "")
                              "\n"))))
    (hash-set m (first bag) (second bag))))

(let loop ((bag target))
  (let ((m (hash-ref data bag)))
    (+ (if (equal? bag target) 0 1)
       (apply + (map (lambda (e) (* (cdr e) (loop (car e)))) (hash->list m))))))