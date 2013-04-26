#lang racket
(require 2htdp/image)
(provide (all-defined-out))

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

;; Position of Left Curl button
;(define L-CURL-POS ())
;; Position of Right Curl button
;(define R-CURL-POS ())

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

;; Right arrow, for indicating curl (rotated for left curl)
(define R-ARROW
         (beside (rectangle PPF (* .25 PPF) 'solid 'white)
                 (rotate -90 (triangle (* .75 PPF) 'solid 'white))))

;; Broom head (for sweeping)
(define BROOM-IMG (ellipse (/ SCR-HT-p 3) (* .5 PPF) 'solid 'red))

;; set-curl-img : String -> Image
;; Where curl is one of "left" or "right"
;; Draws the image for setting curl with 2 buttons
;; Button for curl in given direction is red; otherwire gray
(define (set-curl-img curl)
  (frame
   (overlay/offset
    (overlay (rotate 180 R-ARROW)
             (rectangle (* 2.25 PPF)(* 1.5 PPF) 'solid
                        (if (string=? curl "left")
                            'red 'dimgray)))
    (* .1 WIDTH-p) (* -.125 SCR-HT-p)
    (overlay/offset
     (overlay R-ARROW (rectangle (* 2.25 PPF)(* 1.5 PPF) 'solid
                                 (if (string=? curl "right")
                                     'red 'dimgray)))
     (* -.1 WIDTH-p) (* -.125 SCR-HT-p)
     (overlay/offset
      (text "Curl:" (round (* 1.25 PPF)) 'black)
      0 (* .25 SCR-HT-p)
      (rectangle (* .5 WIDTH-p) SCR-HT-p 'solid 'white))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
