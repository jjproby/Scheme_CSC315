#lang racket


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

(define (get-P0 points)
  (car points))

(define (get-P1 points)
  (car (cdr points)))

(define x (make-list-of-points 10))
(define P0 (get-P0 x))
(define P1 (get-P1 x))

(define (eucDistance x1 y1 x2 y2)
  (define (x-value x2 x1)
    (- x2 x1))
  (define (y-value y2 y1)
    (- y2 y1))
  (define (a-value x)
    (expt x 2))
  (define (b-value y)
    (expt y 2))
  (sqrt (+ (a-value (x-value x2 x1)) (b-value (y-value y2 y1)))))

(define (close-to-P0 P0 P1 points new-list)
  (if (null? points)
      new-list
      (if (<
       (eucDistance (get-x P0) (get-y P0) (get-x (car points)) (get-y (car points)))
       (eucDistance (get-x P1) (get-y P1) (get-x (car points)) (get-y (car points))))
          (close-to-P0 P0 P1 (cdr points) (cons (car points) new-list))
          (close-to-P0 P0 P1 (cdr points) new-list))))

(define (close-to-P1 P0 P1 points new-list)
  (if (null? points)
      new-list
      (if (<
       (eucDistance (get-x P1) (get-y P1) (get-x (car points)) (get-y (car points)))
       (eucDistance (get-x P0) (get-y P0) (get-x (car points)) (get-y (car points))))
          (close-to-P1 P0 P1 (cdr points) (cons (car points) new-list))
          (close-to-P1 P0 P1 (cdr points) new-list))))