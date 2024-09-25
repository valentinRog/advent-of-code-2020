#lang racket

(define rules (hash
               "byr" (lambda (v)
                       (match (list (string->number v) (string-length v))
                         ((list n 4) (and (>= n 1920) (<= n 2002)))
                         (_ #f)))
               "iyr" (lambda (v)
                       (match (list (string->number v) (string-length v))
                         ((list n 4) (and (>= n 2010) (<= n 2020)))
                         (_ #f)))
               "eyr" (lambda (v)
                       (match (list (string->number v) (string-length v))
                         ((list n 4) (and (>= n 2020) (<= n 2030)))
                         (_ #f)))
               "hgt" (lambda (v)
                       (match (string-length v)
                         (l #:when (<= l 2) #f)
                         (l (match (list
                                    (string->number (substring v 0 (- l 2)))
                                    (substring v (- l 2)))
                              ((list #f _) #f)
                              ((list n "cm") (and (>= n 150) (<= n 193)))
                              ((list n "in") (and (>= n 59) (<= n 76)))
                              (_ #f)))))
               "hcl" (lambda (v)
                       (and
                        (equal? (string-length v) 7)
                        (let ((l (string->list v)))
                          (and
                           (equal? (car l) #\#)
                           (andmap (lambda (c) (or
                                                (and (char>=? c #\a) (char<=? c #\f))
                                                (and (char>=? c #\0) (char<=? c #\9))))
                                   (cdr l))))))
               "ecl" (lambda (v) (set-member? (set "amb" "blu" "brn" "gry" "grn" "hzl" "oth") v))
               "pid" (lambda (v) (and
                                  (equal? (string-length v) 9)
                                  (andmap
                                   (lambda (c) (and (char>=? c #\0) (char<=? c #\9)))
                                   (string->list v))))))


(define (valid? passport)
  (let
      ((data (filter
              (lambda (e) (not (equal? (first e) "cid")))
              (map (lambda (e) (string-split e ":")) (string-split passport)))))
    (and
     (equal? (length data) (hash-count rules))
     (andmap
      (lambda (e) ((hash-ref rules (first e)) (second e)))
      data))))

(count valid? (string-split
               (string-replace (string-trim (port->string (current-input-port))) "\r" "")
               "\n\n"))
