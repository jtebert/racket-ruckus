#lang class/2
(require "curling-constants.rkt")
(provide (all-defined-out))

;; Vectors and Posns are accessory classes for Curling.
;; Vectors are used to represent velocity
;;    with an x and y component
;; Posns represent positions
;;    with an x and y location

#|---------------------------------------
            ACCESSORY CLASSES
---------------------------------------|#

;; A Posn is a (new posn% Number Number)
;; A Posn has a x and y coordinate, representing a position
(define-class posn%
  (fields x y)
  
  ;; distance : Posn -> Number
  ;; Distance between this and that Posns
  (define (distance that)
    (sqrt (+ (sqr (- (this . x) (that . x)))
             (sqr (- (this . y) (that . y))))))
  (check-expect (p2 . distance p0) 5)
  (check-within (p1 . distance p0) 14.14 .01)
  (check-expect (p1 . distance p1) 0)
  
  ;; to-from-vel : Posn Number -> Vector
  ;;; Make a Posn going from THIS towards THAT at a given speed.
  ;;> This won't actually work right now because of the differences between
  ;;> Vectors and Posns.  I'll fix then when anything works.
  (define (to-from-vel that speed)
    ((this . sub that) . scale
                       (/ speed (this . distance that))))
  
  ;; find-normal : Posn -> Vector
  ;; Create a vector with components of the differences between this and that
  (define (find-normal that)
    (new vector%
         (- (that . x) (this . x))
         (- (that . y) (this . y))))
  (check-expect (p2 . find-normal p0)
                (new vector% -3 -4))
  (check-expect (p1 . find-normal p0)
                (new vector% -10 -10))
  (check-expect (p1 . find-normal p1)
                (new vector% 0 0))
  (check-expect (p2 . find-normal p1)
                (new vector% 7 6)))

;; Examples:
(define p0 (new posn% 0 0))
(define p1 (new posn% 10 10))
(define p2 (new posn% 3 4))

;; This button (center of house) (0,0)
(define THIS-BUTTON-POS (new posn% CNTR-POS D2B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Vector is a (new vec% Number Number)
;; Which represents a vector of a velocity, giving the x and y components
(define-class vector%
  (fields x y)
  
  ;; magnitude : -> Number
  ;; Get the magnitude (length) of the Vector
  (define (magnitude)
    (sqrt (+ (sqr (this . x))
             (sqr (this . y)))))
  (check-expect (vec1 . magnitude) 5)
  (check-expect (vec0 . magnitude) 0)
  
  ;; dot : Vector -> Number
  ;; Produce the dot product of this and that (a scalar)
  (define (dot that)
    (+ (* (this . x) (that . x))
       (* (this . y) (that . y))))
  (check-expect (vec1 . dot vec0) 0)
  (check-expect (vec0 . dot vec1) 0)
  (check-expect (vec1 . dot vec2) 70)
  (check-expect (vec2 . dot vec1) 70)
  
  ;; add : Vector -> Vector
  ;; Addition of 2 vectors
  (define (add that)
    (new vector%
         (+ (this . x) (that . x))
         (+ (this . y) (that . y))))
  (check-expect (vec0 . add vec1) vec1)
  (check-expect (vec1 . add vec0) vec1)
  (check-expect (vec1 . add vec2)
                (new vector% 13 14))
  (check-expect (vec2 . add vec1)
                (new vector% 13 14))
  
  ;; scale : Number -> Vector
  ;; Scale velocity vector by a scalar number
  (define (scale num)
    (new vector% (* (this . x) num)
         (* (this . y) num)))
  (check-expect (vec0 . scale 10) vec0)
  (check-expect (vec1 . scale 0) vec0)
  (check-expect (vec1 . scale -1)
                (new vector% -3 -4))
  (check-expect (vec2 . scale 10)
                (new vector% 100 100))
  
  ;; sub : Vector -> Vector
  ;; Vector subtraction of that from this
  (define (sub that)
    (this . add (that . scale -1)))
  (check-expect (vec1 . sub vec0) vec1)
  (check-expect (vec0 . sub vec1)
                (new vector% -3 -4))
  (check-expect (vec1 . sub vec2)
                (new vector% -7 -6))
  (check-expect (vec2 . sub vec1)
                (new vector% 7 6))
  
  ;; unit : -> Vector
  ;; Get the unit vector by dividing the vector by its magnitude
  (define (unit)
    (this . scale (/ 1 (this . magnitude))))
  (check-expect (vec1 . unit)
                (new vector% 3/5 4/5))
  (check-within (vec2 . unit)
                (new vector% 0.707 0.707) .01)
  
  ;; normal : -> Vector
  ;; Find a vector normal to this
  (define (normal)
    (new vector%
         (* -1 (this . y))
         (this . x)))
  (check-expect (vec0 . normal) vec0)
  (check-expect (vec1 . normal)
                (new vector% -4 3))
  (check-expect (vec2 . normal)
                (new vector% -10 10))
  
  ;; comp-normal : Vector -> Number
  ;; A scalar of the component of the vector in the direction of the normal
  (define (comp-normal norm)
    ((norm . unit) . dot this))
  (check-expect (vec0 . comp-normal vec1) 0)
  (check-within (vec3 . comp-normal vec3) (sqrt 2) 0.001)
  (check-within (vec1 . comp-normal vec3) (sqrt 24.5) 0.001)
  (check-expect (vec3 . comp-normal vec1) 7/5)
  (check-expect (vec1 . comp-normal (new vector% 4 -3)) 0)
  (check-within (vec3 . comp-normal vec2) (sqrt 2) 0.001)
  
  ;; comp-tangent Vector : -> Vector
  ;; A scalar of the component of the vector tangential to the normal
  (define (comp-tangent norm)
    ((norm . unit . normal) . dot this))
  (check-expect (vec0 . comp-tangent vec1) 0)
  (check-expect (vec1 . comp-tangent vec1) 0)
  (check-within (vec3 . comp-tangent vec2) 0 0.001)
  (check-expect (vec3 . comp-tangent vec1) -1/5)
  (check-within (vec1 . comp-tangent vec2) (sqrt 0.5) 0.001)
  
  ;; col-comp-normal : Vector Vector Number Number -> Number
  ;; Produce the normal component of this after colliding with that
  ;; this has mass m1; that has mass m2
  ;; norm is the normal vector between the 2 objects
  ;; For curling, masses will always be equal (at 19kg)
  (define (col-comp-normal norm that m1 m2)
    (/ (+ (* (this . comp-normal norm) (- m1 m2))
          (* 2 m2 (that . comp-normal norm)))
       (+ m1 m2)))
  (check-expect (vec0 . col-comp-normal vec1 vec1 10 10) 5)
  (check-expect (vec0 . col-comp-normal vec1 vec1 5 10) 20/3)
  (check-expect (vec2 . col-comp-normal vec1 vec1 5 10) 2)
  (check-expect (vec2 . col-comp-normal vec1 vec3 5 10) -14/5)
  
  ;; col-vel/norm : Vector Vector Number Number -> Vector
  ;; Compute the velocity of this after colliding with that
  ;; norm is the normal vector between the 2 objects
  (define (col-vel/norm norm that m1 m2)
    ((norm . unit . scale
           (this . col-comp-normal norm that m1 m2))
     . add
     (norm . normal . unit . scale
           (this . comp-tangent norm))))
  (check-expect (vec0 . col-vel/norm vec1 vec1 10 10)
                (new vector% 3 4))
  (check-expect (vec2 . col-vel/norm vec1 vec0 5 10)
                (new vector% -6/5 -74/15))
  (check-expect (vec2 . col-vel/norm vec1 vec1 10 10)
                (new vector% 23/5 14/5))
  
  ;; col-vel : Posn Posn Vector Number Number -> Vector
  ;; Compute velocity of this after colliding with that
  ;; given the positions of this (p1) and that (p2)
  (define (col-vel p1 p2 that m1 m2)
    (col-vel/norm (p1 . find-normal p2) that m1 m2))
  (check-expect (vec0 . col-vel p1 p2 vec0 10 10)
                (new vector% 0 0))
  (check-within (vec1 . col-vel p1 p2 vec1 10 10)
                (new vector% 3 4) 0.001)
  
  ;; acc : Number -> Vector
  ;; Accelerate the vector by the given acceleration
  (define (acc a)
    (this . scale (/ (+ (this . magnitude) a)
                     (this . magnitude))))
  (check-expect (vec1 . acc 0) vec1)
  (check-within (vec2 . acc 1)
                (new vector% (+ 10 (sqrt .5)) (+ 10 (sqrt .5))) 0.01)
  (check-expect (vec1 . acc 1)
                (new vector% 18/5 24/5)))

;; Examples:
(define vec0 (new vector% 0 0))
(define vec1 (new vector% 3 4))
(define vec2 (new vector% 10 10))
(define vec3 (new vector% 1 1))