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
    (for/list ((block (string-split raw "\n\n")))
      (parse-tile (cdr (string-split block "\n"))))))

(define tile-size (add1 (real-part (max-by magnitude (hash-keys (car data))))))

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

(define (transpose tile)
  (for/hash ((z (hash-keys tile)))
    (values (make-rectangular (imag-part z) (real-part z)) (hash-ref tile z))))

(define (v-flip tile)
  (for/hash ((z (hash-keys tile)))
    (values (make-rectangular (- (- tile-size 1) (real-part z)) (imag-part z)) (hash-ref tile z))))

(define (rotate tile) (v-flip (transpose tile)))

(define (tile->string tile)
  (for/fold ((acc "")) ((y (in-range tile-size)))
    (string-append
     (for/fold ((acc acc)) ((x (in-range tile-size)))
       (string-append acc (string (hash-ref tile (make-rectangular x y)))))
     "\n")))

(define (configurations tile)
  (list tile
        (rotate tile)
        (rotate (rotate tile))
        (rotate (rotate (rotate tile)))
        (v-flip tile)
        (v-flip (rotate tile))
        (v-flip (rotate (rotate tile)))
        (v-flip (rotate (rotate (rotate tile))))))
