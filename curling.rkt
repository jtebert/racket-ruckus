#lang class/2
(require 2htdp/image class/universe)

#|---------------------------------------
               CONSTANTS
---------------------------------------|#

;; PHYSICS CONSTANTS

;; Number of pixels per foot in real curling dimensions
(define PPF 20)
;; Frictional force
;; Effect of sweep (reduce curl, increase velocity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIMENSIONS (in pixels)

;; Diameter of House (12')
(define HOUSE-D (* 12 PPF))
;; Radius of 12' (6')
(define R-12FT (/ HOUSE-D 2))
;; Radius of 8' (4')
(define R-8FT (* 4 PPF))
;; Radius of 4' (2')
(define R-4FT (* 2 PPF))
;; Radius of 1' (6")
(define R-1FT (* .5 PPF))
;; Width of sheet (14'2")
(define WIDTH (* (+ 14 2/12) PPF))
;; Length of sheet (146')
(define LENGTH (* 146 PPF))
;; Distance between hog lines
(define MIDDLE-L (* 72 PPF))
;; Distance from t to end of sheet
(define D2B (* 16 PPF))
;; Radius of stone
(define STONE-R (* .5 PPF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POSITIONS (in pixels)
;; All positions are relative to the top of the sheet (descriptives are from center of this house)
;; THAT refers to throwing end of sheet
;; THIS refers to target end of sheet

;; Center line position (x-coordinate)
(define CNTR-POS (/ WIDTH 2))

;; This t-line (0')
(define THIS-TLINE-POS D2B)
;; This back-line (-6')
(define THIS-BACKLINE-POS (+ D2B (* -.5 HOUSE-D)))
;; This hack (-12')
(define THIS-HACK-POS (+ D2B (* -12 PPF)))
;; This hog line(21')
(define THIS-HOG-POS (+ D2B (* 21 PPF)))

;; That t-line (114')
(define THAT-TLINE-POS (+ D2B (* 114 PPF)))
;; That back-line (120')
(define THAT-BACKLINE-POS (+ D2B (* 120 PPF)))
;; That hack (126')
(define THAT-HACK-POS (+ D2B (* 126 PPF)))
;; That hog line(93')
(define THAT-HOG-POS (+ D2B (* 93 PPF)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OTHER VALUES

;; Starting set of stones

;; Number of ends
(define NUM-ENDS 8)
;; Number of stones per end
(define NUM-STONES 8)
;; Dimensions of curling sheet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMAGES

;; The House (target)
(define HOUSE-IMG
  (overlay (circle R-1FT 'solid 'white)
           (overlay (circle R-4FT 'solid 'red)
                    (overlay (circle R-8FT 'solid 'white)
                             (circle R-12FT 'solid 'blue)))))



;; Sheet - entire playing surface/in-bounds area
; center line, this t-line, this hog line, this back line, 
; that t-line, that hog line, that back line, that hack,
; this house, that house, ice bg
(define SHEET-IMG
  (add-line
   (add-line
    (add-line
     (add-line
      (add-line
       (add-line
        (add-line
         (place-image HOUSE-IMG
                      CNTR-POS THIS-TLINE-POS
                      (place-image HOUSE-IMG
                                   CNTR-POS THAT-TLINE-POS
                                   (rectangle WIDTH LENGTH 'solid 'white)))
         0 THAT-BACKLINE-POS WIDTH THAT-BACKLINE-POS 'black)
        0 THAT-HOG-POS WIDTH THAT-HOG-POS 'black)
       0 THAT-TLINE-POS WIDTH THAT-TLINE-POS 'black)
      0 THIS-BACKLINE-POS WIDTH THIS-BACKLINE-POS 'black)
     0 THIS-HOG-POS WIDTH THIS-HOG-POS 'black)
    0 THIS-TLINE-POS WIDTH THIS-TLINE-POS 'black)
   CNTR-POS 0 CNTR-POS LENGTH 'black))


SHEET-IMG
;; Scoreboard
(define SCOREBOARD
  (rectangle WIDTH (* 4.5 PPF) 'solid 'white))
(define BOX (rectangle PPF (* 1.5 PPF) 'outline 'black))
;; Team 1 stone
;; Team 2 stone
;; Broom head (for sweeping)

#|---------------------------------------
            ACCESSORY CLASSES
---------------------------------------|#

;; A Posn is a (new posn% Number Number)
(define-class posn%
  (fields x y))


;; A Score is a (new scores% String Number)
;; Where name is the name of the team whose scored
;;   and score is the number of points scored in that end
(define-class score%
  (fields name score)
  #|
  ;; total-score : -> Number
  ;; Compute the total score for a team
  (define (total-score)
    (foldr (λ (s b) (+ s b))
           0
           (this . scores)))
  (check-expect (score1 . total-score) 6)
  (check-expect (score2 . total-score) 0)
  
  ;; draw : Number Image -> Image
  ;; Where line is 1 or 2
  ;; Draw the scores on the scoreboard onto a given scene (scoreboard)
  
  ;; add-score : Number -> Score
  ;; Add the new score to the end of the list of scores
  (define (add-score n)
    (new score%
         (this . name)
         (foldr (λ (s b) (cons s b))
                (cons n empty)
                (this . scores))))
  
  (check-expect (score1 . add-score 10)
                (new score% "A" (list 1 2 3 10)))
  (check-expect (score2 . add-score 10)
                (new score% "B" (list 10)))
|#
  )

;; Examples
(define score1
  (new score% "A" (list 1 2 3)))
(define score2
  (new score% "B" empty))

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


;; Abstract super class for Stones
;; A SuperStone is a (new super-stone% Posn Posn Number Number)
;; Where pos is the Stone's position
;;   and vel is the Stone's velocity
;;   and curl is the amount the stone is curling (degrees)
;;   and rotation is how far the stone is rotated from straight
(define-class super-stone%
  (fields pos vel curl rot)
  
  ;; draw/color : Image -> Image
  ;; Draw an Image of the Stone with the given color
  
  ;; off-sheet? : -> Boolean
  ;; Is the stone off of the sheet?
  
  ;; move : -> Stone
  ;; Move the stone according to its velocity and curl
  
  ;; stopped? : -> Boolean
  ;; Is the stone stopped? (Below a certain velocity threshold)
  
  ;; distance-to-button : -> Number
  ;; How far is the Stone from the center of the house?
  
  ;; hogged : -> Boolean
  ;; Did the stone stop before crossing the hog line?
  
  ;; in-house? : -> Boolean
  ;; Is the Stone in the house?
  )

;; Stone for team 1
;; A Team1Stone is a (new team1-stone% Posn Posn Number Number)
(define-class team1-stone%
  (super super-stone%)
  
  ;; draw : Image -> Image
  ;; Draw an Image of the Stone onto the given scene
  )

;; Stone for team 2
;; A Team2Stone is a (new team2-stone% Posn Posn Number Number)
(define-class team2-stone%
  (super super-stone%)
  
  ;; draw : Image -> Image
  ;; Draw an Image of the Stone onto the given scene
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
;; An EndWorlds is a (new end-worlds% Scores [Listof Stone] [Listof Stone])]
;; Where scores contains each team's scores
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
(define-class end-worlds%
  (fields scores thrown-stones unthrown-stones))

;; A World in which the player is throwing a Stone
;; A ThrowWorld is a
;; (new throw-world% Scores [Listof Stone] [Listof Stone] Stone)
;; Where scores contains each team's scores
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and throw-stone is the stone being thrown
(define-class throw-world%
  (super end-worlds%)
  (fields throw-stone)
  
  ;; to-draw : -> Image
  ;; Current stone in main area
  ;; Full sheet on side, complete with all stones
  ;; Scores
  ;; Slider to set curl
  
  ;; on-tick : -> World
  ;; Produce the next world state
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; drag on bar to change curl; drag and release stone to throw
  
  )

;; A World in which player is directing thrown stone
;; A SlideWorld is a
;; (new throw-world% Scores [Listof Stone] [Listof Stone] Stone)
;; Where scores contains each team's scores
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and slide-stone is the active sliding stone
(define-class slide-stone%
  (super end-worlds%)
  (fields slide-stone)
  
  ;; to-draw : -> Image
  ;; Current stone centered in window
  ;; Full sheet on side, complete with all stones
  ;; Scores
  ;; Indicator showing velocity
  
  ;; on-tick : -> World
  ;; Produce the next world state (moving stone)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Click and drag to sweep in front of the stone
  )

;; A world showing the house between throws
;; A BetweenThrowWorld is a (new between-throw-world% Scores [ListofStone] [Listof Stone])
;; Where scores contains each team's scores
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
;; A BetweenEndsWorld is a (new between-ends-world% Scores)
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