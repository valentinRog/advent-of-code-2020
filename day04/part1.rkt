#lang racket

(define fields (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define (valid? passport)
  (equal?
   (set-intersect
    (list->set (map (lambda (e) (car (string-split e ":"))) (string-split passport)))
    fields)
   fields))

(count valid? (string-split
               (string-replace (string-trim (port->string (current-input-port))) "\r" "")
               "\n\n"))
