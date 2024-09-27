#lang racket

(apply + (map (lambda (e) (set-count (list->set (string->list (string-replace e "\n" "")))))
              (string-split
               (string-replace (string-trim (port->string (current-input-port))) "\r" "")
               "\n\n")))