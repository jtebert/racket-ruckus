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
(define PPM 60)
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
#|
;; Deceleration per tick (m/tick^2)
(define TICK-ACC (* ACC-FRIC (sqr TICK-RATE)))
|#
;; Effect of sweep (reduce curl, increase velocity)
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

;; Height of game window
(define GAME-HEIGHT (* 2 WIDTH))
(define GAME-HEIGHT-p (* GAME-HEIGHT PPM))
;; Scaling factor for small sheet
(define SCALE (/ GAME-HEIGHT LENGTH))
;; Width of game window
(define GAME-WIDTH
  (+ WIDTH (* WIDTH SCALE)))
(define GAME-WIDTH-p (* GAME-WIDTH PPM))

;; Scoreboard
;; Board makes the background of red, white, and blue (helper)
(define BOARD
  (place-image (rectangle GAME-WIDTH-p (* 1.5 PPF) 'solid 'blue)
               (* .5 GAME-WIDTH-p) (* 0.75 PPF)
               (place-image (rectangle GAME-WIDTH-p (* 1.5 PPF) 'solid 'red)
                            (* .5 GAME-WIDTH-p) (* 3.75 PPF)
                            (rectangle GAME-WIDTH-p (* 4.5 PPF) 'solid 'white))))

;; Scoreboard is the board with lines and numbers
(define SCOREBOARD
  (local [(define (add-num-line n base) ;; add lines to the background
            (local [(define x (- GAME-WIDTH-p (* n SCR-W-p)))]
              (add-line
               (place-image (text (number->string (- MAX-SCORE (sub1 n)))
                                  (round PPF) 'black)
                            (+ x (/ SCR-W-p 2)) (* 2.25 PPF)
                            base)
               x 0 x (* 4.5 PPF) 'black)))]
    (foldr add-num-line
           BOARD
           (build-list MAX-SCORE
                       (λ (n) (add1 n))))))

;; Broom head (for sweeping)
(define BROOM-IMG (ellipse (* 1.5 PPF) (* .5 PPF) 'solid 'red))

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
  
  ;; draw-scores : String String -> Image
  ;; Draw the scores onto the scoreboard (team1 on top, team2 on bottom)
  (define (draw-scores team1 team2)
    (local [(define name-width (* 0.9 (- GAME-WIDTH (* MAX-SCORE SCR-W))))
            (define (team-txt team)
              ;; Create image of team name, no wider than name-width
              (foldr (λ (n base)
                       (if (< (image-width (text team n 'white))
                              name-width)
                           base
                           (text team n 'white)))
                     (text team MPF 'white)
                     (build-list MPF (λ (x) (add1 x)))))
            (define (draw-score scr bg)
              (place-image
               (overlay (text (number->string (scr . end)) MPF 'black)
                        (rectangle (* .9 MPF) (* .9 SCR-W) 'solid 'white))
               (+ (- GAME-WIDTH (* (- MAX-SCORE (sub1 (scr . score))) SCR-W))
                  (/ SCR-W 2))
               (cond [(string=? (scr . name) team1) (* 0.75 MPF)]
                     [(string=? (scr . name) team2) (* 3.75 MPF)]
                     [else (* 7 MPF)])
               bg))]
      (place-image (team-txt team1)
                   (/ (- GAME-WIDTH (* SCR-W MAX-SCORE)) 2) (* 0.75 MPF)
                   (place-image (team-txt team2)
                                (/ (- GAME-WIDTH (* SCR-W MAX-SCORE)) 2)
                                (* 3.75 MPF)
                                (foldr draw-score
                                       SCOREBOARD
                                       (this . scores)))))))

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


;; A SuperStone is a (new stone% String Posn Vector Number Number)
;; Where team is the name of the team whose stone this is
;;   and pos is the Stone's position
;;   and vel is the Stone's velocity
;;   and curl is the amount the stone is curling (degrees)
;;   and rotation is how far the stone is rotated from straight
(define-class stone%
  (fields team pos vel curl rot)
  
  ;; draw/color : Symbol Image -> Image
  ;; Draw an Image of the Stone with the given color
  (define (draw/color color scn)
    (place-image (overlay (circle (* .65 STONE-R) 'solid color)
                          (circle STONE-R 'solid 'gray))
                 (this . pos . x)
                 (this . pos . y)
                 scn))
  
  ;; off-sheet? : -> Boolean
  ;; Is the stone off of the sheet?
  (define (off-sheet?)
    (or (> (- (this . pos . x) STONE-R) WIDTH)
        (< (+ (this . pos . x) STONE-R) 0)
        (< (+ (this . pos . y) STONE-R) THIS-BACKLINE-POS)))
  
  ;; move : -> Stone
  ;; Move the stone according to its velocity and curl
  (define (move)
    (new stone%
         (this . team)
         (+ (this . pos . x) (this . vel . x))
         (+ (this . pos . y) (this . vel . y))
         (this . curl)
         (this . vel . acc ACC-FRIC)))
  ;;______ Needs change due to curl
  
  ;; stopped? : -> Boolean
  ;; Is the stone stopped? (Below a certain velocity threshold)
  (define (stopped?)
    (< (this . vel . magnitude) STOP-THRESH))
  
  ;; distance-to-button : -> Number
  ;; How far is the Stone from the center of the house?
  (define (distance-to-button)
    (this . distance (new posn% CNTR-POS D2B)))
  
  ;; hogged : -> Boolean
  ;; Did the stone stop before fully crossing the hog line?
  (define (hogged?)
    (and (this . stopped?)
         (> (+ (this . pos . y) STONE-R)
            THIS-TLINE-POS)))
  
  ;; in-house? : -> Boolean
  ;; Is the Stone in the house?
  (define (in-house?)
    (<= (this . distance-to-button) R-12FT))
  )

;; A Stones is a (new stones% [Listof Stone])
(define-class stones%
  (fields stones)
  
  ;; draw-all : Image -> Image
  ;; Draw all of the stones in the list onto the given scene
  (define (draw-all scn)
    (foldr (λ (stn base)
             (place-image (stn . draw)
                          (stn . pos . x) (stn . pos . y)
                          base))
           scn
           (this . stones)))
  
  ;; sitting : -> [or String false]
  ;; Return the name of the team with Stone closest to the button
  ;; Return false if no stones are in the house
  (define (sitting)
    (local [(define clst (this . closest))]
      (cond [(boolean? clst) clst]
            [else (clst . name)])))
  
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
  
  ;; closest/team : String -> Stone
  ;; Return the Stone closest to the center of the house for the given team
  
  
  ;; total-sitting : -> Number
  ;; Return the the total number of points for the team that is sitting
  ;; How many stones they have closer to the button than any of their
  ;; opponent's stones
  #;(define (total-sitting)
    (local [(define s-team (this . stones . sitting))
            (define closest-opp)]
      ()))
  )

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

;; An abstract World for all worlds during an end
;; An EndWorlds is a (new end-worlds% [Listof Score] Stones Stones)]
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
(define-class end-worlds%
  (fields scores thrown-stones unthrown-stones))

;; A World in which the player is throwing a Stone
;; A ThrowWorld is a
;; (new throw-world% Scores Stones Stones Stone String)
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and throw-stone is the stone being thrown
;;   and curl is the direction of the curl ("left" or "right")
(define-class throw-world%
  (super end-worlds%)
  (fields throw-stone goal-x curl)
  
  ;; to-draw : -> Image
  ;; Current stone in main area
  ;; Full sheet on side, complete with all stones
  ;; Scores
  ;; Indicator of curl direction
  
  ;; on-tick : -> World
  ;; Produce the next world state
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Mouse x position is the target of the stone (at this t-line)
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
                           10)))] ;; Temporary constant speed
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
        this))
  )

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
  
  ;; to-draw : -> Image
  ;; Current stone centered in window
  ;; Full sheet on side, complete with all stones
  ;; Scores
  ;; Indicator showing velocity
  (define (to-draw)
    (local [(define scaled-sheet
              (this . thrown-stones . draw-all
                    (scale SCALE SHEET-IMG)))
            (define main-sheet
              (this . unthrown-stones . draw-all 
                    (this . slide-stone . draw 
                          SHEET-IMG)))]
      (place-image scaled-sheet
                   (/ (image-width scaled-sheet) 2)
                   (/ GAME-HEIGHT 2)
                   (place-image main-sheet
                                (+ (image-width scaled-sheet) (/ WIDTH 2))
                                (/ GAME-HEIGHT 2)
                                (empty-scene GAME-WIDTH GAME-HEIGHT)))))
  
  ;; on-tick : -> World
  ;; Produce the next world state (moving stone)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Click and drag to sweep in front of the stone
  )

;; A world showing the house between throws
;; A BetweenThrowWorld is a (new between-throw-world% Scores Stones Stones)
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
(define-class between-throw-world%
  (super end-worlds%)
  ;; to-draw : -> Image
  ;; House
  ;; Full sheet on side, complete with all stones
  ;; Scores
  
  ;; on-tick : -> World
  ;; Same world (is this necessary?)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Click to switch to throwing new stone
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