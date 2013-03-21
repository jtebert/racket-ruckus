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
;; Maximum score on scoreboard
(define MAX-SCORE 12)
;; Width of a space on the scoreboard
(define SCR-W (* PPF 1.25))

;; Width of game window
(define GAME-WIDTH (* 1.5 WIDTH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMAGES

;; The House (target)
(define HOUSE-IMG
  (overlay (circle R-1FT 'solid 'white)
           (overlay (circle R-4FT 'solid 'red)
                    (overlay (circle R-8FT 'solid 'white)
                             (circle R-12FT 'solid 'blue)))))



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
(define BOARD
  (place-image (rectangle GAME-WIDTH (* 1.5 PPF) 'solid 'blue)
               (* .5 GAME-WIDTH) (* 0.75 PPF)
               (place-image (rectangle GAME-WIDTH (* 1.5 PPF) 'solid 'red)
                            (* .5 GAME-WIDTH) (* 3.75 PPF)
                            (rectangle GAME-WIDTH (* 4.5 PPF) 'solid 'white))))

(define SCOREBOARD
  (local [(define (add-num-line n base)
            (local [(define x (- GAME-WIDTH (* n SCR-W)))]
              (add-line (place-image (text (number->string (- MAX-SCORE (sub1 n))) PPF 'black)
                                     (+ x (/ SCR-W 2)) (* 2.25 PPF)
                                     base)
                        x 0 x (* 4.5 PPF) 'black)))]
    (foldr add-num-line
           BOARD
           (build-list MAX-SCORE
                       (λ (n) (add1 n))))))
SCOREBOARD
;; Team 1 stone
;; Team 2 stone
;; Broom head (for sweeping)
(define BROOM-IMG (ellipse (* 1.5 PPF) (* .5 PPF) 'solid 'red))
BROOM-IMG

#|---------------------------------------
            ACCESSORY CLASSES
---------------------------------------|#

;; A Posn is a (new posn% Number Number)
(define-class posn%
  (fields x y)
  
  ;; total : -> Number
  ;; Total distance from 0 (if vel, net velocity
  (define (total)
    (sqrt (+ (sqr (this . x)) (sqr (this . y)))))
  )


;; A Score is a (new score% Number String Number)
;; Where end is the number of the end in the game
;;   and name is the name of the team whose scored (false means no score)
;;   and score is the number of points scored
(define-class score%
  (fields end name score))

;; Examples
(define score1
  (new score% 1 "A" 4))
(define score2
  (new score% 3 "B" 1))

;; A Scores is a (new scores% [Listof Score])
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
  
  ;; winner : String String -> String
  ;; Return the name of the team with the most points
  ;; Given the names of the 2 teams
  (define (winner team1 team2)
    (if (> (this . total-score(team1))
           (this . total-score(team2)))
        team1
        team2))
  
  ;; draw-scores : String String -> Image
  ;; Draw the scores onto the scoreboard (team1 on top, team2 on bottom)
  (define (draw-scores team1 team2)
    (local [(define name-width (* 0.9 (- GAME-WIDTH (* MAX-SCORE SCR-W))))
            (define (team-txt team) ;; Create image of team name, no wider than name-width
              (foldr (λ (n base)
                       (if (< (image-width (text team n 'white))
                              name-width)
                           base
                           (text team n 'white)))
                     (text team PPF 'white)
                     (build-list PPF (λ (x) (add1 x)))))
            (define (draw-score scr bg)
              (place-image (overlay (text (number->string (scr . end)) PPF 'black)
                                    (rectangle (* .9 PPF) (* .9 SCR-W) 'solid 'white))
                           (+ (- GAME-WIDTH (* (- MAX-SCORE (sub1 (scr . score))) SCR-W)) (/ SCR-W 2))
                           (cond [(string=? (scr . name) team1) (* 0.75 PPF)]
                                 [(string=? (scr . name) team2) (* 3.75 PPF)]
                                 [else (* 7 PPF)])
                           bg))]
      (place-image (team-txt team1)
                   (/ (- GAME-WIDTH (* SCR-W MAX-SCORE)) 2) (* 0.75 PPF)
                   (place-image (team-txt team2)
                                (/ (- GAME-WIDTH (* SCR-W MAX-SCORE)) 2) (* 3.75 PPF)
                                (foldr draw-score
                                       SCOREBOARD
                                       (this . scores))))))
  )

;; Examples:
(define scores1 (new scores%
                     (list score1 score2)))

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
  
  ;; draw/color : Symbol Image -> Image
  ;; Draw an Image of the Stone with the given color
  (define (draw/color color scn)
    (overlay (circle (* .65 STONE-R) 'solid color)
             (circle STONE-R 'solid 'gray)))
  
  ;; off-sheet? : -> Boolean
  ;; Is the stone off of the sheet?
  (define (off-sheet?)
    (or (> (- (this . pos . x) STONE-R) WIDTH)
        (< (+ (this . pos . x) STONE-R) 0)
        (< (+ (this . pos . y) STONE-R) THIS-BACKLINE-POS)))
  
  ;; move : -> Stone
  ;; Move the stone according to its velocity and curl
  
  ;; stopped? : -> Boolean
  ;; Is the stone stopped? (Below a certain velocity threshold)
  (define (stopped?)
    (< (this . vel . total) 0.5))
  
  ;; distance-to-button : -> Number
  ;; How far is the Stone from the center of the house?
  (define (distance-to-button)
    ((new posn% ((+ THIS-TLINE-POS (this . pos . x)) (+ THIS-TLINE-POS (this . pos . y))))
     . vel . total))
  
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
;; An EndWorlds is a (new end-worlds% [Listof Score] [Listof Stone] [Listof Stone])]
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
(define-class end-worlds%
  (fields scores thrown-stones unthrown-stones))

;; A World in which the player is throwing a Stone
;; A ThrowWorld is a
;; (new throw-world% Scores [Listof Stone] [Listof Stone] Stone)
;; Where scores is a list of who scored in each end
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
;; Where scores is a list of who scored in each end
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