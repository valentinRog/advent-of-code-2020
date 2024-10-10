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

(define (compute)
  (let loop
    ((ingredients (set-subtract (list->set ingredients) target-ingredients))
     (allergens allergens)
     (acc '()))
    (cond
      ((not (andmap (lambda (e) (possible-allergen? (car e) (cdr e))) acc)) #f)
      ((set-empty? ingredients) acc)
      (#t (for/or ((allergen (set->list allergens)))
            (let ((ingredient (set-first ingredients)))
              (loop (set-remove ingredients ingredient)
                    (set-remove allergens allergen)
                    (append acc (list (cons ingredient allergen))))))))))

(displayln
 (string-join (map car (sort (compute) (lambda (e1 e2) (string<? (cdr e1) (cdr e2))))) ","))