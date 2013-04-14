#lang class/2
(require 2htdp/image class/universe)

;; All physics calculations are computed using SI units (meters, s, kg)
;; All drawing is converted into pixels
;; Conventiently, all dimensions for curling are in feet

#|---------------------------------------
               CONSTANTS
---------------------------------------|#

;; PHYSICS CONSTANTS

;; Number of pixels per meter in real curling dimensions
(define PPM 65)
;; Feet to meters conversion
(define MPF .3048)
;; Convert feet to pixels
(define PPF (* PPM MPF))
;; Tick rate (s/tick)
(define TICK-RATE 1/28)

;; Threshold for velocity below which is considered stopped (m/s)
(define STOP-THRESH 0.05)

;; Gravity (-9.81 m/s^2)
(define GRAV -9.81)
;; Mass of stone (~19 kg)
(define STONE-MASS 19)
;; Normal force of stone/ice
(define F-NORM (* STONE-MASS GRAV))
;; Coefficient of friction (0.0168)
(define FRIC 0.0168)
;; Frictional force (N)
(define F-FRIC (* FRIC F-NORM))
;; Acceleration (deceleration) due to friction (m/s^2)
;; a = F/m
(define ACC-FRIC (/ F-FRIC STONE-MASS))
;; Effect of sweep
;; (reduce curl, increase velocity by reducing co-efficient of friction)
;; _________

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIMENSIONS (in pixels)

;; Diameter of House (12')
(define HOUSE-D (* 12 MPF))
(define HOUSE-D-p (* HOUSE-D PPM))
;; Radius of 12' (6')
(define R-12FT (/ HOUSE-D 2))
(define R-12FT-p (* R-12FT PPM))
;; Radius of 8' (4')
(define R-8FT (* 4 MPF))
(define R-8FT-p (* R-8FT PPM))
;; Radius of 4' (2')
(define R-4FT (* 2 MPF))
(define R-4FT-p (* R-4FT PPM))
;; Radius of 1' (6")
(define R-1FT (* .5 MPF))
(define R-1FT-p (* R-1FT PPM))
;; Width of sheet (14'2")
(define WIDTH (* (+ 14 2/12) MPF))
(define WIDTH-p (* WIDTH PPM))
;; Length of sheet (146')
(define LENGTH (* 146 MPF))
(define LENGTH-p (* LENGTH PPM))
;; Distance between hog lines
(define MIDDLE-L (* 72 MPF))
(define MIDDLE-L-p (* MIDDLE-L PPM))
;; Distance from t to end of sheet
(define D2B (* 16 MPF))
(define D2B-p (* D2B PPM))
;; Radius of stone
(define STONE-R (* .5 MPF))
(define STONE-R-p (* STONE-R PPM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POSITIONS (in pixels)
;; All positions are relative to the top left of the sheet
;; (descriptives are from center of this house)
;; THAT refers to throwing end of sheet
;; THIS refers to target end of sheet

;; Center line position (x-coordinate)
(define CNTR-POS (/ WIDTH 2))
(define CNTR-POS-p (* CNTR-POS PPM))

;; This t-line (0')
(define THIS-TLINE-POS D2B)
(define THIS-TLINE-POS-p D2B-p)
;; This back-line (-6')
(define THIS-BACKLINE-POS (+ D2B (* -.5 HOUSE-D)))
(define THIS-BACKLINE-POS-p (* THIS-BACKLINE-POS PPM))
;; This hack (-12')
(define THIS-HACK-POS (+ D2B (* -12 MPF)))
(define THIS-HACK-POS-p (* THIS-HACK-POS PPM))
;; This hog line (21')
(define THIS-HOG-POS (+ D2B (* 21 MPF)))
(define THIS-HOG-POS-p (* THIS-HOG-POS PPM))

;; That t-line (114')
(define THAT-TLINE-POS (+ D2B (* 114 MPF)))
(define THAT-TLINE-POS-p (* THAT-TLINE-POS PPM))
;; That back-line (120')
(define THAT-BACKLINE-POS (+ D2B (* 120 MPF)))
(define THAT-BACKLINE-POS-p (* THAT-BACKLINE-POS PPM))
;; That hack (126')
(define THAT-HACK-POS (+ D2B (* 126 MPF)))
(define THAT-HACK-POS-p (* THAT-HACK-POS PPM))
;; That hog line(93')
(define THAT-HOG-POS (+ D2B (* 93 MPF)))
(define THAT-HOG-POS-p (* THAT-HOG-POS PPM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OTHER VALUES

;; Starting set of stones

;; Number of ends
(define NUM-ENDS 8)
;; Number of stones per end
(define NUM-STONES 8)
;; Maximum score on scoreboard
(define MAX-SCORE 12)
;; Width of a space on the scoreboard
(define SCR-W (* MPF 1.25))
(define SCR-W-p (* SCR-W PPM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FOR DRAWING FINAL OUTPUT
;; All sizes in pixels

;; Scoreboard height
(define SCR-HT-p (* 4.5 PPF))
;; Scoreboard width
(define SCR-WIDTH-p (* 1.5 WIDTH-p))
;; Height of visible area of sheet (back line to hog line)
(define VIEW-HT-p (+ (- THIS-HOG-POS-p THIS-BACKLINE-POS-p) (* 3 STONE-R-p)))
;; Height of game window
(define GAME-HT-p (+ SCR-HT-p VIEW-HT-p))
;; Scaling factor for small sheet
(define SCALE (/ GAME-HT-p LENGTH-p))
;; Width of small sheet
(define SM-WIDTH-p (* SCALE WIDTH-p))
;; Width of game window
(define GAME-WIDTH-p (+ (* 2 WIDTH-p) SM-WIDTH-p))

;; Background on which to place all the other stuff
(define BG (rectangle GAME-WIDTH-p GAME-HT-p 'solid 'gray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMAGES

;; The House (target)
(define HOUSE-IMG
  (overlay (circle R-1FT-p 'solid 'white)
           (overlay (circle R-4FT-p 'solid 'red)
                    (overlay (circle R-8FT-p 'solid 'white)
                             (circle R-12FT-p 'solid 'blue)))))

;; Sheet - entire playing surface/in-bounds area
(define SHEET-IMG
  (add-line
   (add-line
    (add-line
     (add-line
      (add-line
       (add-line
        (add-line
         (place-image HOUSE-IMG
                      CNTR-POS-p THIS-TLINE-POS-p
                      (place-image HOUSE-IMG
                                   CNTR-POS-p THAT-TLINE-POS-p
                                   (rectangle WIDTH-p LENGTH-p 'solid 'white)))
         0 THAT-BACKLINE-POS-p WIDTH-p THAT-BACKLINE-POS-p 'black)
        0 THAT-HOG-POS-p WIDTH-p THAT-HOG-POS-p 'black)
       0 THAT-TLINE-POS-p WIDTH-p THAT-TLINE-POS-p 'black)
      0 THIS-BACKLINE-POS-p WIDTH-p THIS-BACKLINE-POS-p 'black)
     0 THIS-HOG-POS-p WIDTH-p THIS-HOG-POS-p 'black)
    0 THIS-TLINE-POS-p WIDTH-p THIS-TLINE-POS-p 'black)
   CNTR-POS-p 0 CNTR-POS-p LENGTH-p 'black))

;; Scoreboard is the board with lines and numbers
(define SCOREBOARD
  (local
    [(define BOARD
       (overlay/xy (rectangle SCR-WIDTH-p (/ SCR-HT-p 3) 'solid 'blue)
                   0 0
                   (overlay/xy (rectangle SCR-WIDTH-p (/ SCR-HT-p 3) 'solid 'red)
                               0 (* -2/3 SCR-HT-p)
                               (rectangle SCR-WIDTH-p SCR-HT-p 'solid 'white))))
     (define (add-num-line n base) ;; add lines to the background
       (local [(define x (- SCR-WIDTH-p (* n SCR-W-p)))]
         (add-line
          (place-image (text (number->string (- MAX-SCORE (sub1 n)))
                             (round PPF) 'black)
                       (+ x (/ SCR-W-p 2)) (/ SCR-HT-p 2)
                       base)
          x 0 x SCR-HT-p 'black)))]
    (foldr add-num-line
           BOARD
           (build-list MAX-SCORE
                       (λ (n) (add1 n))))))

;; Broom head (for sweeping)
(define BROOM-IMG (ellipse (/ SCR-HT-p 3) (* .5 PPF) 'solid 'red))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Score is a (new score% Number String Number)
;; Represents the score of a team from a particular end
;; Where end is the number of the end in the game
;;   and name is the name of the team whose scored (false means no score)
;;   and score is the number of points scored
(define-class score%
  (fields end name score))

;; Examples
(define score1 (new score% 1 "A" 4))
(define score2 (new score% 3 "B" 1))
(define score3 (new score% 8 "B" 8))

;; A Scores is a (new scores% [Listof Score])
;; Represents the scores of all ends of the game thus far
;; Where each element of scores is the score of an end, in order
;; If an end # is not in the list, it means that was a blank end
(define-class scores%
  (fields scores)
  
  ;; total-score : String -> Number
  ;; Return the total score of the given team
  (define (total-score team)
    (foldr (λ (n base)
             (if (string=? (n . name) team)
                 (+ (n . score) base)
                 base))
           0 (this . scores)))
  (check-expect (scores1 . total-score "A") 4)
  (check-expect (scores1 . total-score "B") 1)
  (check-expect (scores1 . total-score "C") 0)
  (check-expect (scores2 . total-score "B") 9)
  
  ;; winner : String String -> String
  ;; Return the name of the team with the most points
  ;; Given the names of the 2 teams
  (define (winner team1 team2)
    (cond [(> (this . total-score team1)
              (this . total-score team2)) team1]
          [else team2]))
  (check-expect (scores1 . winner "A" "B") "A")
  (check-expect (scores2 . winner "A" "B") "B")
  (check-expect (scores1 . winner "C" "B") "B")
  
  ;; draw : String String -> Image
  ;; Draw the scores onto the scoreboard (team1 on top, team2 on bottom)
  (define (draw team1 team2)
    (local [(define name-width (* 0.9 (- SCR-WIDTH-p (* MAX-SCORE SCR-W))))
            ;; Create image of team name, no wider than name-width
            (define (team-txt team)
              (foldr (λ (n base)
                       (if (< (image-width (text team n 'white))
                              name-width)
                           base
                           (text team n 'white)))
                     (text team (round PPF) 'white)
                     (build-list (round PPF) (λ (x) (add1 x)))))
            ;; draw scores onto the base image
            (define (draw-scores scrs bg t1-scr t2-scr)
              (cond [(empty? scrs) bg]
                    [else (place-image
                            (overlay (text (number->string ((first scrs) . end)) (round PPF) 'black)
                                     (rectangle (* .9 PPF) (* .9 SCR-W-p) 'solid 'white))
                            (+ (- SCR-WIDTH-p
                                  (* (- MAX-SCORE
                                        (+ (if (string=? ((first scrs) . name) team1) t1-scr t2-scr)
                                           (sub1 ((first scrs) . score))))
                                     SCR-W-p))
                               (/ SCR-W-p 2))
                           (cond [(string=? ((first scrs) . name) team1) (* 1/6 SCR-HT-p)]
                                 [(string=? ((first scrs) . name) team2) (* 5/6 SCR-HT-p)]
                                 [else (* 7/6 SCR-HT-p)])
                           (draw-scores (rest scrs) bg
                                        (if (string=? ((first scrs) . name) team1)
                                            (+ ((first scrs) . score) t1-scr) t1-scr)
                                        (if (string=? ((first scrs) . name) team2)
                                            (+ ((first scrs) . score) t2-scr) t2-scr)))]))]
      (place-image (team-txt team1)
                   (/ (- SCR-WIDTH-p (* SCR-W-p MAX-SCORE)) 2) (* 1/6 SCR-HT-p)
                   (place-image (team-txt team2)
                                (/ (- SCR-WIDTH-p (* SCR-W-p MAX-SCORE)) 2)
                                (* 5/6 SCR-HT-p)
                                (draw-scores (this . scores) SCOREBOARD 0 0))))))

;; Examples:
(define scores1 (new scores%
                     (list score1 score2)))
(define scores2 (new scores% 
                     (list score1 score2 score3)))


#|---------------------------------------
                 STONES
---------------------------------------|#

;; A Stone implements:
;; - draw : Image -> Image
;;   Draw an Image of the Stone onto the given scene
;; - off-sheet? : -> Boolean
;;   Is the stone off of the sheet?
;; - move : -> Stone
;;   Move the stone according to its velocity and curl
;; - stopped? : -> Boolean
;;   Is the stone stopped? (Below a certain velocity threshold)
;; - distance-to-button : -> Number
;;   How far is the Stone from the center of the house?
;; - hogged : -> Boolean
;;   Did the stone stop before crossing the hog line?
;; - in-house? : -> Boolean
;;   Is the Stone in the house?


;; A Stone is a (new stone% String Posn Vector Number Number)
;; Where team is the name of the team whose stone this is
;;   and pos is the Stone's position
;;   and vel is the Stone's velocity
;;   and curl is the amount the stone is curling (degrees)
;;   and rotation is how far the stone is rotated from straight
(define-class stone%
  (fields team pos vel curl rot)
  
  ;; draw/color : String Image -> Image
  ;; Draw an Image of the Stone with the given color
  ;; at the correct location and rotation
  (define (draw/color color scn)
    (place-image (rotate (this . curl)
                         (overlay/offset
                          (overlay/align/offset
                           "middle" "top"
                           (ellipse (* .4 STONE-R-p) (* .4 STONE-R-p) 'solid color)
                           0 (* .1 STONE-R-p)
                           (ellipse (* .3 STONE-R-p) STONE-R-p 'outline 'black))
                          0 (* .1 STONE-R-p)
                          (overlay (circle (* .65 STONE-R-p) 'solid color)
                                   (circle STONE-R-p 'solid 'gray))))
                 (* (this . pos . x) PPM)
                 (* (this . pos . y) PPM)
                 scn))
  
  ;; off-sheet? : -> Boolean
  ;; Is the stone off of the sheet?
  ;; Completely part the back line OR touching one of the sides
  ;;> Might need corection for back end of sheet
  (define (off-sheet?)
    (or (> (+ (this . pos . x) STONE-R) WIDTH)
        (< (- (this . pos . x) STONE-R) 0)
        (< (+ (this . pos . y) STONE-R) THIS-BACKLINE-POS)))
  (check-expect (stone0 . off-sheet?) true)
  (check-expect (stone1 . off-sheet?) false)
  (check-expect (stone5 . off-sheet?) true)
  (check-expect (stone4 . off-sheet?) true)
  (check-expect (stone3 . off-sheet?) false)
  
  ;; move : -> Stone
  ;; Move the stone according to its velocity and curl
  ;; Set velocity to 0 if stone is below stop threshold
  (define (move)
    (if (this . stopped?)
        (new stone%
             (this . team)
             (this . pos . x) (this . pos . y)
             (this . curl)
             (new vector% 0 0))
        (new stone%
             (this . team)
             (+ (this . pos . x) (this . vel . x))
             (+ (this . pos . y) (this . vel . y))
             (this . curl)
             (this . vel . acc ACC-FRIC))))
  ;;> Needs change due to curl
  
  ;; stopped? : -> Boolean
  ;; Is the stone stopped? (Below a certain velocity threshold)
  (define (stopped?)
    (< (abs (this . vel . magnitude)) STOP-THRESH))
  (check-expect (stone0 . stopped?) true)
  (check-expect (stone1 . stopped?) false)
  (check-expect (stone2 . stopped?) true)
  
  ;; distance-to-button : -> Number
  ;; How far is the Stone from the center of the house?
  (define (distance-to-button)
    (this . pos . distance THIS-BUTTON-POS))
  (check-within (stone0 . distance-to-button)
                (sqrt (+ (sqr (THIS-BUTTON-POS . x))
                         (sqr (THIS-BUTTON-POS . y))))
                0.01)
  (check-within (stone1 . distance-to-button)
                (sqrt (+ (sqr (- (THIS-BUTTON-POS . x) (stone1 . pos . x)))
                         (sqr (- (THIS-BUTTON-POS . y) (stone1 . pos . y)))))
                0.01)
  
  ;; hogged? : -> Boolean
  ;; Did the stone stop before fully crossing the hog line?
  (define (hogged?)
    (and (this . stopped?)
         (> (+ (this . pos . y) STONE-R)
            THIS-HOG-POS)))
  (check-expect (stone1 . hogged?) false)
  (check-expect (stone2 . hogged?) false)
  (check-expect (stone3 . hogged?) false)
  (check-expect (stone4 . hogged?) true)
  
  ;; in-house? : -> Boolean
  ;; Is the Stone in the house?
  ;; Any part of the stone tourching the house is in the house
  (define (in-house?)
    (<= (this . distance-to-button) (+ STONE-R R-12FT)))
  (check-expect (stone0 . in-house?) false)
  (check-expect (stone1 . in-house?) true)
  (check-expect (stone3 . in-house?) false))

;; Examples
(define stone0 (new stone% "A" (new posn% 0 0) vec0 0 0))
(define stone1 (new stone% "B" (new posn% 2 5) vec1 45 45))
(define stone2 (new stone% "B" (new posn% 2 5) (new vector% 0.01 0.01) 45 45))
(define stone3 (new stone% "A" (new posn% 3 15) (new vector% -1 -1) 10 100))
(define stone4 (new stone% "A" (new posn% .1 15) (new vector% 0 0) 10 100))
(define stone5 (new stone% "B" (new posn% 4.25 5) (new vector% 0 0) 10 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Stones is a (new stones% [Listof Stone])
(define-class stones%
  (fields stones)
  
  ;; draw-all : String Image -> Image
  ;; Draw all of the stones in the list onto the given scene
  ;; The stones of the given team are red; other team is blue
  (define (draw-all team scn)
    (foldr (λ (stn base)
             (if (string=? team (stn . team))
                 (stn . draw/color 'red base)
                 (stn . draw/color 'blue base)))
           scn
           (this . stones)))
  
  ;; move-all : -> Stones
  ;; Move all of the stones in the set
  (define (move-all)
    (new stones% (map (λ (s )(s . move)) (this . stones))))
  
  ;; sitting : -> [or String false]
  ;; Return the name of the team with Stone closest to the button
  ;; Return false if no stones are in the house
  (define (sitting)
    (local [(define clst (this . closest))]
      (cond [(boolean? clst) clst]
            [else (clst . team)])))
  (check-expect (stones0 . sitting) false)
  (check-expect (stones3 . sitting) false)
  (check-expect (stones1 . sitting) "B")
  (check-expect (stones2 . sitting) "B")
  
  ;; in-house/team : String -> Stones
  ;; Return the stones of a given team that are in the house
  (define (in-house/team team)
    (new stones% (filter (λ (s) (and (s . in-house?)
                                     (string=? team (s . team))))
                         (this . stones))))
  (check-expect (stones0 . in-house/team "A") stones0)
  (check-expect (stones0 . in-house/team "B") stones0)
  (check-expect (stones1 . in-house/team "B")
                (new stones% (list stone1 stone2)))
  (check-expect (stones1 . in-house/team "A") stones0)
  (check-expect (stones2 . in-house/team "A")
                (new stones% (list (new stone% "A" (new posn% 3 6) vec0 0 0))))
  (check-expect (stones2 . in-house/team "B")
                (new stones% (list (new stone% "B" (new posn% 2 5) vec0 0 0)
                                   (new stone% "B" (new posn% 3 5) vec0 0 0)
                                   (new stone% "B" (new posn% 2 3.5) vec0 0 0))))
  
  ;; in-house/other-team : String -> Stones
  ;; Return the stones of the OTHER team that are in the house
  ;; aka the stones where the team is not the given team
  (define (in-house/other-team team)
    (new stones% (filter (λ (s) (and (s . in-house?)
                                     (not (string=? team (s . team)))))
                         (this . stones))))
  (check-expect (stones0 . in-house/other-team "A") stones0)
  (check-expect (stones0 . in-house/other-team "B") stones0)
  (check-expect (stones1 . in-house/other-team "A")
                (new stones% (list stone1 stone2)))
  (check-expect (stones1 . in-house/other-team "B") stones0)
  (check-expect (stones2 . in-house/other-team "B")
                (new stones% (list (new stone% "A" (new posn% 3 6) vec0 0 0))))
  (check-expect (stones2 . in-house/other-team "A")
                (new stones% (list (new stone% "B" (new posn% 2 5) vec0 0 0)
                                   (new stone% "B" (new posn% 3 5) vec0 0 0)
                                   (new stone% "B" (new posn% 2 3.5) vec0 0 0))))
  
  ;; closest : -> [or false Stone]
  ;; Find the closest stone to the center of the house
  ;; Return false if none are in the house
  (define (closest)
    (local [(define (get-closer s base)
              (cond [(and (s . in-house?)
                          (or (boolean? base)
                              (< (s . distance-to-button)
                                 (base . distance-to-button))))
                     s]
                    [else base]))]
      (foldr get-closer false (this . stones))))
  (check-expect (stones0 . closest) false)
  (check-expect (stones3 . closest) false)
  (check-expect (stones2 . closest) (new stone% "B" (new posn% 2 5) vec0 0 0))
  (check-expect (stones1 . closest) stone2)
  
  ;; closest/other-team : String -> [or false Stone]
  ;; Return the Stone closest to the center of the house for the OTHER team
  ;; aka stone of NOT the given team
  ;; return false if none are in the house
  (define (closest/other-team team)
    (this . in-house/other-team team . closest))
  (check-expect (stones0 . closest/other-team "A") false)
  (check-expect (stones0 . closest/other-team "B") false)
  (check-expect (stones1 . closest/other-team "A") stone2)
  (check-expect (stones1 . closest/other-team "B") false)
  (check-expect (stones2 . closest/other-team "B")
                (new stone% "A" (new posn% 3 6) vec0 0 0))
  (check-expect (stones2 . closest/other-team "A")
                (new stone% "B" (new posn% 2 5) vec0 0 0))
  (check-expect (stones3 . closest/other-team "A") false)
  (check-expect (stones3 . closest/other-team "B") false)
  
  ;; total-sitting : -> Number
  ;; Return the the total number of points for the team that is sitting
  ;; How many stones they have closer to the button than any of their
  ;; opponent's stones
  (define (total-sitting)
    (local [;; name of the team that's sitting (or false if no stones in house)
            (define s-team (this . sitting))
            ;; stones of the team that's sitting (that are in the house)
            (define s-stones (this . in-house/team s-team))
            ;; closest stone of the team that's not sitting (or false)
            (define closest-opp (this . closest/other-team s-team))
            (define (if-closer s base)
              (cond [(or (boolean? closest-opp)
                         (< (s . distance-to-button)
                            (closest-opp . distance-to-button)))
                     (add1 base)]
                    [else base]))]
      (foldr if-closer 0 (s-stones . stones))))
  (check-expect (stones0 . total-sitting) 0)
  (check-expect (stones1 . total-sitting) 2)
  (check-expect (stones2 . total-sitting) 3)
  (check-expect (stones3 . total-sitting) 0))

;; Examples
(define stones0 (new stones% empty))
(define stones1 (new stones%
                     (list stone1 stone2 stone3 stone4 stone5)))
(define stones2 (new stones%
                     (list (new stone% "B" (new posn% 2 5) vec0 0 0)
                           (new stone% "A" (new posn% 3 6) vec0 0 0)
                           (new stone% "B" (new posn% 3 5) vec0 0 0)
                           (new stone% "B" (new posn% 2 3.5) vec0 0 0)
                           (new stone% "B" (new posn% 5 6) vec0 0 0)
                           (new stone% "B" (new posn% 5 7) vec0 0 0))))
(define stones3 (new stones% (list stone0)))

#|---------------------------------------
                  WORLDS
---------------------------------------|#

;; A World implements:
;; - to-draw : -> Image
;;   Draw the elements of the World
;; - on-tick : -> World
;;   Produce the next world state

#|---------------------------------------
                END WORLDS
---------------------------------------|#

;; An abstract World for all worlds during an end:
;; - ThrowWorld (throwing stone)
;; - SlideWorld (while stone is sliding)

;; An EndWorlds is a (new end-worlds% [Listof Score] Stones Stones)]
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and curl is the direction of the stone's curl ('left or 'right)
(define-class end-worlds%
  (fields scores thrown-stones unthrown-stones curl)
  
  ;; to-draw : -> Image
  ;; Current stone centered left window
  ;; Scoreboard at the bottom
  ;; House on the left
  ;; Overview of sheet on far left
  ;;> Need velocity indicator
  (define (to-draw)
    (local
      [(define sheet/stones
         (this . thrown-stones . draw-all
               (this . unthrown-stones . draw-all
                     (this . slide-stone . draw SHEET-IMG))))
       (define scaled-sheet (frame (scale SCALE sheet/stones)))
       (define target-sheet
         (frame (crop 0 (- THIS-BACKLINE-POS-p (* 2 STONE-R-p))
                      WIDTH-p VIEW-HT-p
                      sheet/stones)))
       (define view-sheet
         (frame (crop 0
                      (min (max (- THIS-BACKLINE-POS-p (* 2 STONE-R-p))
                                (this . slide-stone . pos . y))
                           (- LENGTH-p VIEW-HT-p))
                      WIDTH-p VIEW-HT-p
                      sheet/stones)))
       (define scores-img
         (frame (this . scores . draw)))
       (define r-arrow
         (beside (rectangle PPF (* .25 PPF) 'solid 'white)
                 (rotate -90 (triangle (* .75 PPF) 'solid 'white))))
       (define set-curl-img
         (frame
          (overlay/offset
           (overlay (rotate 180 r-arrow)
                    (rectangle (* 2.25 PPF)(* 1.5 PPF) 'solid
                               (if (symbol=? (this . curl) 'left)
                                   'red 'dimgray)))
           (* .1 WIDTH-p) (* -.125 SCR-HT-p)
           (overlay/offset
            (overlay r-arrow (rectangle (* 2.25 PPF)(* 1.5 PPF) 'solid
                                        (if (symbol=? (this . curl) 'left)
                                            'red 'dimgray)))
            (* -.1 WIDTH-p) (* -.125 SCR-HT-p)
            (overlay/offset
             (text "Curl:" (round (* 1.25 PPF)) 'black)
             0 (* .25 SCR-HT-p)
             (rectangle (* .5 WIDTH-p) SCR-HT-p 'solid 'white))))))]
      (place-image
       view-sheet
       (/ WIDTH-p 2) (/ VIEW-HT-p 2)
       (place-image
        target-sheet
        (+ (/ WIDTH-p 2) WIDTH-p) (/ VIEW-HT-p 2)
        (place-image
         scaled-sheet
         (+ (/ (image-width scaled-sheet) 2) (* 2 WIDTH-p)) (/ GAME-HT-p 2)
         (place-image
          scores-img
          (/ SCR-WIDTH-p 2) (+ (/ SCR-HT-p 2) VIEW-HT-p)
          (place-image
           set-curl-img
           (* 1.75 WIDTH-p) (+ (* .5 SCR-HT-p) VIEW-HT-p)
           BG))))))))

;; A World in which the player is throwing a Stone
;; A ThrowWorld is a
;; (new throw-world% Scores Stones Stones Stone String)
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and throw-stone is the stone being thrown
;;   and curl is the direction of the curl ('left or 'right)
(define-class throw-world%
  (super end-worlds%)
  (fields throw-stone goal-x)
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> If stone being dragged, move it back
  ;;> else this
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Mouse x position is the target of the stone (at this t-line)
  ;;> If click on buttons: set curl
  ;;> If mouse over target sheet:
  ;;>    show position of goal there
  ;;> If mouse over view sheet:
  ;;>    show position of goal there/target sheet
  ;;> If click and drag while within view/target sheets:
  ;;>    pull stone back so increase velocity
  ;;> If release mouse (in this region):
  ;;>    Set velocity (based on distance back from hack/start) and throw (new slide-world% ...)
  (define (on-mouse me x y)
    (cond [(mouse=? me "button-down")
           (new slide-world%
                (this . scores)
                (this . thrown-stones)
                (this . unthrown-stones)
                (new stone%
                     (this . throw-stone . team)
                     (this . throw-stone . pos)
                     (this . throw-stone . to-from-vel
                           (new posn% (this . goal-x) D2B)
                           10)))] ;; ____ Temporary constant speed
          [else (new throw-world%
                     (this . scores)
                     (this . thrown-stones)
                     (this . unthrown-stones)
                     (this . throw-stone)
                     x
                     (this . curl))]))
  
  ;; on-key : KeyEvent -> World
  ;; Left and right arrows set curl of stone
  (define (on-key ke)
    (if (or (string=? ke "right")
            (string=? ke "left"))
        (new throw-world%
             (this . scores)
             (this . thrown-stones)
             (this . unthrown-stones)
             (this . throw-stone)
             (this . goal-x)
             ke)
        this)))

;; A World in which player is directing thrown stone
;; A SlideWorld is a
;; (new throw-world% Scores Stones Stones Stone)
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and slide-stone is the active sliding stone
(define-class slide-world%
  (super end-worlds%)
  (fields slide-stone)
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> Move the current stone
  ;;> Check for and act upon any collisions:
  ;;>    Within the set of thrown stones
  ;;>    Between the slide-stone and thrown-stones
  ;;> If all stones (slide-stone and thrown-stones) are stopped
  ;;>    (new throw-world%) , move next stone into position
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Click and drag to sweep in front of the stone
  )

#|---------------------------------------
               OTHER WORLDS
---------------------------------------|#

;; A World between ends
;; A BetweenEndsWorld is a (new between-ends-world% [Listof Score])
;; Where scores is a list of who scored in each end
(define-class between-ends-world%
  (fields scores)
  
  ;; to-draw : -> Image
  ;; Draw scores centered on screen
  
  ;; on-tick : -> World
  ;; Same world (is this necessary?)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Click to start new end
  )

;; A World for after the game ends
;; A game-over-world is a (new game-over-world% Scores)
(define-class game-over-world%
  (fields scores)
  
  ;; to-draw : -> Image
  ;; Draw scores centered on screen; say who the winner is
  
  ;; on-tick : -> World
  ;; Same world (is this necessary?)mber Number -> World
  ;; Click to start new end
  )