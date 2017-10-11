#lang racket/gui

;; Define a function here that generates a random
;; floating point number x whose value lies
;; between 0 and 1 and that has no more than
;; two digits to the right of the decimal point.
;; (Multiply by 100, truncate to eliminate the
;; fractional part, and then divide by 100 to
;; get two digits on the right of the decimal point.)
;; This function will have no parameters.

(define (make-coordinate)
  (/ (truncate (* 1000 (random))) 1000 ))

;; Define a function here that generates a list
;; of two random floating point numbers, each
;; of whose values lies between 0 and 1.
;; This function will have no parameters.

(define (make-point)
  (list
   (make-coordinate)
   (make-coordinate)))

;; Define a function here that generates a list of
;; n points. The function's one parameter will be n.
;; Each point will be a list that contains two
;; elements, the x coordinate and the y coordinate.
;; Each coordinate of each point will be a random
;; number in the interval [0,1].

(define (make-list-of-points count)
  (define (helper count result)
    (if (equal? count 0)
        result
        (helper (- count 1)
                (cons (make-point) result))))
  (helper count '()))

;; Write code here that gives the names p0 and p1,
;; respectively, to the first and second points
;; in the list of points.

(define (get-P0 points)
  (car points))

(define (get-P1 points)
  (car (cdr points)))

(define x (make-list-of-points 10))
(define P0 (get-P0 x))
(define P1 (get-P1 x))

;; Define a function here that has one argument.
;; That argument will be a point.
;; The function will return to its caller a negative
;; floating point value if p is closer to p0 than
;; it is to p1.
;; It will return a positive floating point value if
;; p is closer to p1 than it is to p0.
;; It will return 0 if p is equidistant from p0 and p1.

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


(define (closer-to points)
  (cond
    ((<
      (eucDistance (get-x P0) (get-y P0) (get-x (car points)) (get-y (car points)))
       (eucDistance (get-x P1) (get-y P1) (get-x (car points)) (get-y (car points)))) -1)
    ((<
      (eucDistance (get-x P1) (get-y P1) (get-x (car points)) (get-y (car points)))
       (eucDistance (get-x P0) (get-y P0) (get-x (car points)) (get-y (car points)))) 1)
    (0)))
      
        
;; Write code here that creates a sublist from the list
;; of points. This sublist will contain all of the points
;; that are closer to p0 than they are to p1.
;; Call this sublist points-0.

;; Write code here that creates a sublist from the list
;; of points. This sublist will contains all of the points
;; that are closer to p1 than they are to p0.
;; Call this sublist points-1.

;; Here's are two examples of lists of points.
;; When you have written your own code to generate
;; lists named points-0 and points-1, hide these
;; next two lines from the interpreter by placing
;; them in comments.
(define points-0 '((0.75 0.75) (0.25 0.75) (0.25 0.25) (0.75 0.25)))
(define points-1 '((0.5 0.75) (0.25 0.5) (0.5 0.25) (0.75 0.5)))

;; You may use different colors to distinguish
;; the elements of points-0 from the elements of
;; points-1.
(define dot-color-0 "red")
(define dot-color-1 "blue")

;; You may change the size of the picture.
(define picture-size 512)

;; You may change the size of the dots
;; that represent the points.
(define dot-radius 8)
(define dot-diameter (* 2 dot-radius))

;;;; DO NOT CHANGE ANY OF THE CODE BELOW HERE ;;;;

(define (get-x point) (car point))
(define (get-y point) (car (cdr point)))

(define (scale point factor)
  (list
   (exact->inexact (* (get-x point) factor))
   (exact->inexact (* (get-y point) factor))))

(define (make-scale-function divisor)
  (lambda (point) (scale point divisor)))

(define scale-function (make-scale-function picture-size))

(define scaled-points-0 (map scale-function points-0))
(define scaled-points-1 (map scale-function points-1))

(define (point-function point draw-function)
  (let ((cx (get-x point)) (cy (get-y point)))
    (draw-function (- cx dot-radius) (- cy dot-radius) dot-diameter)))

(define (draw-points list-of-points draw-function)
  (if (null? list-of-points)
      #f
      (begin
        (point-function (car list-of-points) draw-function)
        (draw-points (cdr list-of-points) draw-function))
      ))

(define (set-dot-color dc dot-color)
  (send dc set-brush dot-color 'solid))

(define (paint dc list-of-points)
  (draw-points
   list-of-points
   (lambda (ulx uly d) (send dc draw-ellipse ulx uly d d))))

(define (paint-colored-dots dc dot-color list-of-points)
  (begin
    (set-dot-color dc dot-color)
    (paint dc (map scale-function list-of-points))))
  
(define frame (new frame%
                   [label "Example"]
                   [width picture-size]
                   [height picture-size]))
(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        
        (paint-colored-dots dc dot-color-0 points-0)
        
        (paint-colored-dots dc dot-color-1 points-1)
        )])
(send frame show #t)