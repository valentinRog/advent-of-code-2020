#lang racket

(define (string-remove s l) (for/fold ((s s)) ((w l)) (string-replace s w "")))

(define (parse-food s) s
  (let ((l (map string-split (string-split (string-remove s '("contains" "," ")")) "("))))
    (cons (list->set (car l)) (list->set (second l)))))

(define data
  (map parse-food (string-split (string-trim (port->string (current-input-port))) "\n")))


(define ingredients (set->list (apply set-union (map car data))))
(define allergens (set->list (apply set-union (map cdr data))))

(define (possible-allergen? ingredient allergen)
  (andmap
   (lambda (food)
     (match (cons (set-member? (car food) ingredient) (set-member? (cdr food) allergen))
       ((cons #f #t) #f)
       (_ #t)))
   data))

(define target-ingredients
  (list->set
   (filter
    (lambda (ingredient) (andmap
                          (lambda (allergen) (not (possible-allergen? ingredient allergen)))
                          allergens))
    ingredients)))

(for/fold ((acc 0))
          ((food (map car data))) (+ acc (set-count (set-intersect target-ingredients food))))
