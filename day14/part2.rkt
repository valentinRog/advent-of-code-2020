#lang racket

(define (parse-block s)
  (define (left-pad s c)
    (match (string-length s)
      (36 s)
      (_ (left-pad (string-append c s) c))))
  (define (reg-n s) (substring s 4 (- (string-length s) 1)))
  (define (n->bin-string s) (left-pad (number->string (string->number s) 2) "0"))
  (let ((l (string-split s "\n")))
    (cons (second (string-split (car l)))
          (for/list ((e (cdr l)))
            (let ((l (map string-trim (string-split e "="))))
              (cons (n->bin-string (reg-n (car l))) (string->number (second l))))))))

(define data
  (map parse-block (string-split (string-trim (port->string (current-input-port))) "mask")))

(define (compute-addresses mask s)
  (define (map-append-acc acc c) (map (lambda (e) (string-append e c)) acc))
  (for/fold ((acc '(""))) ((mask-b (string->list mask)) (b (string->list s)))
    (match (cons b mask-b)
      ((cons _ #\1) (map-append-acc acc "1"))
      ((cons c #\0) (map-append-acc acc (string c)))
      ((cons _ #\X) (append (map-append-acc acc "0") (map-append-acc acc "1"))))))

(apply + (hash-values
          (for/fold ((acc (make-immutable-hash))) ((block data))
            (for/fold ((acc acc)) ((ins (cdr block)))
              (for/fold ((acc acc)) ((key (compute-addresses (car block) (car ins))))
                (hash-set acc key (cdr ins)))))))
