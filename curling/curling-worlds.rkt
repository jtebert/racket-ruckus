#lang class/2
(require 2htdp/image class/universe "curling-constants.rkt" "posns-vectors.rkt")

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

;; An EndWorlds is a (new end-worlds% [Listof Score] Stones Stones String)]
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and curl is the direction of the stone's curl ("left" or "right")
(define-class end-worlds%
  (fields scores thrown-stones unthrown-stones curl)
  
  ;; to-draw : -> Image
  ;; Current stone centered left window
  ;; Scoreboard at the bottom
  ;; House on the left
  ;; Overview of sheet on far left
  ;;> Need velocity indicator
  ;;> Drawing broom in slide-world?
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
         (frame (this . scores . draw)))]
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
           (set-curl-img (this . curl))
           (* 1.75 WIDTH-p) (+ (* .5 SCR-HT-p) VIEW-HT-p)
           BG))))))))

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
  (fields throw-stone goal-x)
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> If stone being dragged, move it back
  ;;> else this
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Mouse x position is the target of the stone (at this t-line)
  (define (on-mouse me x y)
    (cond [;;> If click on buttons: set curl
           ]
          [;;> If mouse over target sheet:
           ;;>    show position of goal there
           ]
          [;;> If click and drag while within view/target sheets:
           ;;>    pull stone back to increase velocity (Have maximum amount back)
           ]
          [;;> If release mouse (in this region):
           ;;>    Set velocity (based on distance back from hack/start) and throw (new slide-world% ...)
           ]
          [(mouse=? me "button-down")
           (new slide-world%
                (this . scores)
                (this . thrown-stones)
                (this . unthrown-stones)
                (new stone%
                     (this . throw-stone . team)
                     (this . throw-stone . pos)
                     (this . throw-stone . to-from-vel
                           (new posn% (this . goal-x) D2B)
                           10)))] ;;> Temporary constant speed
          [else (new throw-world%
                     (this . scores)
                     (this . thrown-stones)
                     (this . unthrown-stones)
                     (this . throw-stone)
                     x
                     (this . curl))]))
  
  ;; on-key : KeyEvent -> World
  ;; Left and right arrows set curl of stone
  ;;> This might go away (or just be left as alternative to mouse)
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
  ;;> If drag (on target or view sheet):
  ;;>   Effect of sweep is inversely proportional to distance in front of stone
  ;;>                      proportional to speed of sweeping
  ;;> If broom hits stone:
  ;;>    Stone burned; go to (new throw-world% ...)
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
  ;;> Draw scores centered on screen
  ;;> Say click/press any key to continue
  
  ;; on-tick : -> World
  ;;> Same world (is this necessary?)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;;> Click to start new end
  
  ;; on-key : KeyEvent -> World
  ;;> Press any key to continue
  )

;; A World for after the game ends
;; A game-over-world is a (new game-over-world% Scores)
(define-class game-over-world%
  (fields scores)
  
  ;; to-draw : -> Image
  ;;> Draw scores centered on screen; say who the winner is
  ;;> Press key/click to start new game
  
  ;; on-tick : -> World
  ;; Same world (is this necessary?)mber Number -> World
  
  ;; on-key : KeyEvent -> World
  ;;> Start new game if some key pressed
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;;> Click to start new end
  )