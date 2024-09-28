#lang racket

(define data
  (list->vector
   (map
    (lambda (line) (let ((l (string-split line))) (cons (car l) (string->number (second l)))))
    (string-split (string-trim (port->string (current-input-port))) "\n"))))

(define (compute v)
  (let loop ((i 0) (acc 0) (seen (set)))
    (cond
      ((>= i (vector-length v)) acc)
      ((set-member? seen i) #f)
      (#t (let ((seen (set-add seen i)))
            (match (vector-ref v i)
              ((cons "acc" n) (loop (add1 i) (+ acc n) seen))
              ((cons "jmp" n) (loop (+ i n) acc seen))
              ((cons "nop" _) (loop (add1 i) acc seen))))))))

(let loop ((i 0))
  (let ((ins (car (vector-ref data i))) (n (cdr (vector-ref data i))))
    (if (equal? ins "acc") (loop (add1 i))
        (let ((ins (match ins ("jmp" "nop") ("nop" "jmp"))))
          (match (compute (vector-set/copy data i (cons ins n)))
            (#f (loop (add1 i)))
            (acc acc))))))
