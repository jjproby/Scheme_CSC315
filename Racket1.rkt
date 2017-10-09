#lang racket

(define (my-reverse some-list)
     (define (helper a b)
          (if (null? a)
               b
               (helper (cdr a) (cons (car a) b))))
     (helper some-list '()))

(define (make-coordinate)
  (/ (truncate (* 1000 (random))) 1000 ))

(define (make-point)
  (list
   (make-coordinate)
   (make-coordinate)))

(define (get-x point) (car point))
(define (get-y point) (car (cdr point)))

(define (make-list-of-points count)
  (define (helper count result)
    (if (equal? count 0)
        result
        (helper (- count 1)
                (cons (make-point) result))))
  (helper count '()))

(define (quadrant-1 point)
  (and
   (> (get-x point) 0.5)
   (> (get-y point) 0.5)))

(define (quadrant-2 point)
  (and
   (< (get-x point) 0.5)
   (> (get-y point) 0.5)))

(define (quadrant-3 point)
  (and
   (< (get-x point) 0.5)
   (< (get-y point) 0.5)))

(define (quadrant-4 point)
  (and
   (> (get-x point) 0.5)
   (< (get-y point) 0.5)))