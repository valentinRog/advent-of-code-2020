#lang racket

(define data
  (list->vector
   (map
    (lambda (line) (let ((l (string-split line))) (cons (car l) (string->number (second l)))))
    (string-split (string-trim (port->string (current-input-port))) "\n"))))

(let loop ((i 0) (acc 0) (seen (set)))
  (if (set-member? seen i) acc
      (let ((seen (set-add seen i)))
        (match (vector-ref data i)
          ((cons "acc" n) (loop (add1 i) (+ acc n) seen))
          ((cons "jmp" n) (loop (+ i n) acc seen))
          ((cons "nop" _) (loop (add1 i) acc seen))))))