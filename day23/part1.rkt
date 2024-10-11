#lang racket

(struct s-node (v left right))

(define data
  (map string->number (map string (string->list (string-trim (port->string (current-input-port)))))))

(define v (list->vector data))

(define m (for/hash ((n data)) (values n (box (s-node n '() '())))))

(define four-max (take (sort data >) 4))

(define (left node) (s-node-left (unbox node)))
(define (right node) (s-node-right (unbox node)))
(define (val node) (s-node-v (unbox node)))

(for ((i (in-range (vector-length v))))
  (let ((node (hash-ref m (vector-ref v i)))
        (left (hash-ref m (vector-ref v (modulo (- i 1) (vector-length v)))))
        (right (hash-ref m (vector-ref v (modulo (add1 i) (vector-length v))))))
    (set-box! node (s-node (val node) left right))))

(define (drop-3-right node)
  (let ((new-right-node (right (right (right (right node))))))
    (set-box! node (s-node (val node)
                           (left node)
                           new-right-node))
    (set-box! new-right-node (s-node (val new-right-node)
                                     node
                                     (right new-right-node)))))

(define (insert-3-right node inserting-node)
  (let ((right-to-inserting (right node))
        (last-inserting (right (right inserting-node))))
    (set-box! node (s-node
                    (val node)
                    (left node)
                    inserting-node))
    (set-box! right-to-inserting (s-node
                                  (val right-to-inserting)
                                  last-inserting
                                  (right right-to-inserting)))
    (set-box! inserting-node (s-node
                              (val inserting-node)
                              node
                              (right inserting-node)))
    (set-box! last-inserting (s-node
                              (val last-inserting)
                              (left last-inserting)
                              right-to-inserting))))

(define (make-res)
  (car
   (for/fold ((acc (cons "" (right (hash-ref m 1))))) ((_ (in-range (- (length data) 1))))
     (let ((s (car acc)) (node (cdr acc)))
       (cons (string-append s (number->string (val node))) (right node))))))

(define (compute head k)
  (define (find-target c hs)
    (let loop ((i 1))
      (cond
        ((= (- c i) 0) (for/first ((n four-max) #:when (not (set-member? hs n))) n))
        ((set-member? hs (- c i)) (loop (add1 i)))
        (#t (- c i)))))
  (if (= k 0)
      (make-res)
      (let* ((tmp (right head))
             (destination (hash-ref m (find-target (val head) (set (val tmp) (val (right tmp)) (val (right (right tmp))))))))
        (drop-3-right head)
        (insert-3-right destination tmp)
        (compute (right head) (- k 1)))))

(displayln (compute (hash-ref m (car data)) 100))