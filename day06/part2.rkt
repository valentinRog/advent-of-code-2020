#lang racket

(apply
 +
 (map
  (lambda (s) (length (apply set-intersect (map string->list (string-split s)))))
  (string-split
   (string-replace (string-trim (port->string (current-input-port))) "\r" "")
   "\n\n")))