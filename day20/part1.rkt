#lang racket

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define (max-by key l)
  (for/fold ((acc #f)) ((e l)) (cond ((not acc) e)
                                     ((< (key acc) (key e)) e)
                                     (#t acc))))

(define (parse-tile l)
  (for/fold ((acc (make-immutable-hash)))
            ((line l) (y (in-naturals)))
    (for/fold ((acc acc)) ((c (string->list line)) (x (in-naturals)))
      (hash-set acc (make-rectangular x y) c))))

(define data
  (let ((raw (string-remove (string-trim (port->string (current-input-port))) '("Tile " ":" "\r"))))
    (for/hash ((block (string-split raw "\n\n")))
      (let ((l (string-split block "\n")))
        (values (string->number (car l)) (parse-tile (cdr l)))))))

(define tile-size (add1 (real-part (max-by magnitude (hash-keys (car (hash-values data)))))))

(define borders-indexes
  (let* ((r (stream->list (in-range tile-size)))
         (rs (list
              r
              (map (lambda (i) (* i 0+i)) r)
              (map (lambda (i) (+ (* (- tile-size 1) 0+i) i)) r)
              (map (lambda (i) (+ (* i 0+i) (- tile-size 1))) r))))
    (for/fold ((acc '())) ((l rs)) (append acc (list l (reverse l))))))

(define (border-indexes->string tile l)
  (string-join (map (lambda (z) (string (hash-ref tile z))) l) ""))

(define borders
  (for/hash ((tile-id (hash-keys data)))
    (values
     tile-id
     (for/set ((l borders-indexes)) (border-indexes->string (hash-ref data tile-id) l)))))

(define (corner? tile-id)
  (define (count-border s)
    (count (lambda (hs) (set-member? hs s)) (hash-values borders)))
  (= (count (lambda (e) (= e 1)) (map count-border (set->list (hash-ref borders tile-id)))) 4))

(apply * (filter corner? (hash-keys data)))
