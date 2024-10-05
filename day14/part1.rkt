#lang racket

(define (parse-block s)
  (define (left-pad s c)
    (match (string-length s)
      (36 s)
      (_ (left-pad (string-append c s) c))))
  (define (reg-n s) (string->number (substring s 4 (- (string-length s) 1))))
  (define (parse-n s) (left-pad (number->string (string->number s) 2) "0"))
  (let ((l (string-split s "\n")))
    (cons (second (string-split (car l)))
          (for/list ((e (cdr l)))
            (let ((l (map string-trim (string-split e "="))))
              (cons (reg-n (car l)) (parse-n (second l))))))))

(define data
  (map parse-block (string-split (string-trim (port->string (current-input-port))) "mask")))

(define (compute-ins mask s)
  (let ((s (for/fold ((acc "")) ((mask-b (string->list mask)) (b (string->list s)))
             (match (cons b mask-b)
               ((cons _ #\1) (string-append acc "1"))
               ((cons _ #\0) (string-append acc "0"))
               ((cons c #\X) (string-append acc (string c)))))))
    (string->number s 2)))

(apply +
       (hash-values
        (for/fold ((acc (make-immutable-hash))) ((block data))
          (for/fold ((acc acc)) ((ins (cdr block)))
            (hash-set acc (car ins) (compute-ins (car block) (cdr ins)))))))
