#lang class/2

#|---------------------------------------
               CONSTANTS
---------------------------------------|#

;; PHYSICS CONSTANTS
;; Frictional force
;; Effect of sweep (reduce curl, increase velocity)

;; OTHER VALUES
;; Diameter of House
;; Starting set of stones
;; Number of ends
;; Number of stones per end
;; Dimensions of curling sheet

;; IMAGES
;; The House (target)
;; Sheet - entire playing surface/in-bounds area
;; Scoreboard
;; Team 1 stone
;; Team 2 stone
;; Broom head (for sweeping)

#|---------------------------------------
            ACCESSORY CLASSES
---------------------------------------|#

; A Posn is a (new posn% Number Number)
(define-class posn%
  (fields x y))

; A Scores is a (new scores% String [Listof Number] String [Listof Number])
(define-class scores%
  (fields team1-name team1-scores team2-names team2-scores)
  
  ;; total-score : -> Number
  ;; Compute the total score for a team
  
  ;; draw : Image -> Image
  ;; Draw the scores on the scoreboard onto a given scene
  
  ;; winner : -> String
  ;; Return the name of the winning team
  )


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