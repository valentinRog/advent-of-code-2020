#lang racket

(define data
  (reverse
   (let (( l (sort
              (map string->number (string-split (string-trim (port->string (current-input-port)))))
              > )))
     (cons (+ 3 (car l)) l))))

(struct accumulator (prev n1 n3) #:transparent)
(let
    ((res
      (for/fold ((acc (accumulator 0 0 0))) ((j data))
        (match (- j (accumulator-prev acc))
          (1 (accumulator j (add1 (accumulator-n1 acc)) (accumulator-n3 acc)))
          (3 (accumulator j (accumulator-n1 acc) (add1 (accumulator-n3 acc))))))))
  (* (accumulator-n1 res) (accumulator-n3 res)))
