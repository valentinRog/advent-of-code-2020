#lang racket

(struct s-node (v left right))

(define data
  (map string->number (map string (string->list (string-trim (port->string (current-input-port)))))))

(define (list-max l)
  (for/fold ((acc #f)) ((e l))
    (match acc
      (#f e)
      (acc #:when (< acc e) e)
      (_ acc))))

(define m (for/hash ((n data)) (values n (box (s-node n '() '())))))
(define head (box (hash-ref m (car data))))

(define v (list->vector data))

(for ((i (in-range (vector-length v))))
  (let ((node (hash-ref m (vector-ref v i)))
        (left (hash-ref m (vector-ref v (modulo (- i 1) (vector-length v)))))
        (right (hash-ref m (vector-ref v (modulo (add1 i) (vector-length v))))))
    (set-box! node (s-node (s-node-v (unbox node)) left right))))

(define (print) (for/fold ((node (unbox head))) ((_ (in-range (hash-count m))))
                  (display (s-node-v (unbox node))) (s-node-right (unbox node))))

(define (left node) (s-node-left (unbox node)))
(define (right node) (s-node-right (unbox node)))
(define (val node) (s-node-v (unbox node)))

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

(define (find-target i hs)
  (let ((c (s-node-v (unbox (unbox head)))))
    (match (cons (and (hash-has-key? m (- c i)) (not (set-member? hs (- c i)))) (- c i))
      ((cons #f 0) (list-max (filter (lambda (e) (not (set-member? hs e))) (hash-keys m))))
      ((cons #f _) (find-target (add1 i) hs))
      ((cons _ _) (- c i)))))

(define (move)
  (let ((tmp (right (unbox head))))
    (drop-3-right (unbox head))
    (insert-3-right (hash-ref m (find-target 1 (set (val tmp) (val (right tmp)) (val (right (right tmp)))))) tmp)
    (set-box! head (right (unbox head)))
    ))

(for ((_ (in-range 100))) (move))

(print)