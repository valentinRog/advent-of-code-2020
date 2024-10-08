#lang racket

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define (min-by key l)
  (for/fold ((acc #f)) ((e l)) (cond ((not acc) e)
                                     ((> (key acc) (key e)) e)
                                     (#t acc))))

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
  (let ((raw (string-remove (string-trim (port->string (current-input-port))) '("\r"))))
    (for/list ((block (string-split raw "\n\n")))
      (parse-tile (cdr (string-split block "\n"))))))

(define (compute-tile-size tile) (sqrt (hash-count tile)))
(define tile-size (compute-tile-size (car data)))

(define borders-indexes
  (let ((r (stream->list (in-range tile-size))))
    (hash -i r
          -1 (map (lambda (i) (* i 0+i)) r)
          0+i (map (lambda (i) (+ (* (- tile-size 1) 0+i) i)) r)
          1 (map (lambda (i) (+ (* i 0+i) (- tile-size 1))) r))))

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
  (let ((tile-size (compute-tile-size tile)))
    (for/fold ((acc "")) ((y (in-range tile-size)))
      (string-append
       (for/fold ((acc acc)) ((x (in-range tile-size)))
         (string-append acc (string (hash-ref tile (make-rectangular x y)))))
       "\n"))))

(define (configurations tile)
  (list tile
        (rotate tile)
        (rotate (rotate tile))
        (rotate (rotate (rotate tile)))
        (v-flip tile)
        (v-flip (rotate tile))
        (v-flip (rotate (rotate tile)))
        (v-flip (rotate (rotate (rotate tile))))))

(define (connect? tile1 tile2 d)
  (equal? (border-indexes->string tile1 (hash-ref borders-indexes d))
          (border-indexes->string tile2 (hash-ref borders-indexes (* -1 d)))))

(define (same-tile? tile1 tile2) (set-member? (list->set (configurations tile2)) tile1))

(define (find-adjacent tile d)
  (let loop ((data data))
    (match data
      ('() #f)
      ((list head tail ...)
       (match (for/first ((e (configurations head))
                          #:when (and (connect? tile e d) (not (same-tile? tile e)))) e)
         (#f (loop tail))
         (e e))))))

(define (make-connections)
  (let loop ((acc (hash 0 (car data))))
    (displayln (hash-count acc))
    (let (( acc (for/fold ((acc acc)) ((z (hash-keys acc)))
                  (for/fold ((acc acc)) ((zz (list (- z 1) (+ z 1) (- z 0+i) (+ z 0+i))))
                    (if (hash-has-key? acc zz)
                        acc
                        (match (find-adjacent (hash-ref acc z) (- zz z))
                          (#f acc)
                          (e (hash-set acc zz e))))))))
      (if (= (hash-count acc) (length data)) acc (loop acc)))))

(define (normalize-connections connections)
  (let ((z-offset (min-by (lambda (z) (+ (real-part z) (imag-part z))) (hash-keys connections))))
    (for/hash ((z (hash-keys connections))) (values (- z z-offset) (hash-ref connections z)))))

(define (remove-border tile)
  (let ((keys (filter-not
               (lambda (z) (or (= (real-part z) 0)
                               (= (imag-part z) 0)
                               (= (real-part z) (- tile-size 1))
                               (= (imag-part z) (- tile-size 1))))
               (hash-keys tile))))
    (for/hash ((z keys)) (values (- z 1+i) (hash-ref tile z)))))

(define (remove-borders connections)
  (for/hash ((z (hash-keys connections))) (values z (remove-border (hash-ref connections z)))))

(define image
  (let* ((connections (remove-borders (normalize-connections (make-connections))))
         (tile-size (compute-tile-size (first (hash-values connections)))))
    (for/fold ((acc (hash))) ((z-tile (hash-keys connections)))
      (for/fold ((acc acc)) ((z (hash-keys (hash-ref connections z-tile))))
        (hash-set acc (+ z (* z-tile tile-size)) (hash-ref (hash-ref connections z-tile) z))))))


(for ((y (in-range (add1 (imag-part (max-by imag-part (hash-keys image)))))))
  (for ((x (in-range (add1 (real-part (max-by real-part (hash-keys image)))))))
    (display (hash-ref image (make-rectangular x y))))
  (display "\n"))
