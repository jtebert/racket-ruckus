#lang class/2
(require 2htdp/image
         class/universe
         "curling-constants.rkt"
         "posns-vectors.rkt"
         "curling-elements.rkt")

;; Uses posns/vectors, constants, and elements (stones and scores)
;; in creating World classes.

;; Types of Worlds:
;; - ThrowWorld (Stone is being thrown)
;; - SlideWorld (Stone is sliding down the ice)
;; - BetweenEndWorld (After end finished, before next started)
;; - GameOverWorld (After all ends have been played)

#|---------------------------------------
                  WORLDS
---------------------------------------|#

;; A World implements:
;; - to-draw : -> Image
;;   Draw the elements of the World
;; - on-tick : -> World
;;   Produce the next world state

;; An abstract world for all game states
;; A SuperWorld is:
;;   (new s-world% String String Number Scores)
;;   (new s-world% team1 team2 end scores)
;; Where team1 and team2 are the names of the teams in the game
;;   and End is the Number of the end in the game
(define-class s-world%
  (fields team1 team2 end))


#|---------------------------------------
                END WORLDS
---------------------------------------|#

;; An abstract World for all worlds during an end:
;; - ThrowWorld (throwing stone)
;; - SlideWorld (while stone is sliding)

;; An EndWorld is:
;;   (new end-world% Stones Stones String String String Number Scores)
;;   (new end-world% thrown-stones unthrown-stones curl team1 team2 end scores)
;; Where scores is a list of who scored in each end
;;   and thrown-stones is a list of the stones in play at the other end
;;   and unthrown-stones are the stones remaining in the end
;;   and curl is the direction of the stone's curl ("left" or "right")
(define-class end-world%
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
         (frame (this . scores . draw (this . team1) (this . team2))))]
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
;; A ThrowWorld is:
;;   (new throw-world% Stone Number Stones Stones String String String Number Scores)
;;   (new throw-world% throw-stone goal-x thrown-stones unthrown-stones curl team1 team2 end scores)
;; Where throw-stone is the stone being thrown
;;   and goal-x is the x position at the T of the far house the stone will go to
(define-class throw-world%
  (super end-world%)
  (fields throw-stone goal-x)
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> If stone being dragged, move it back
  ;;>>> I think that's taken care of by on-mouse? Need to test/try
  ;;> else this
  (define (on-tick) this)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;; Mouse x position is the target of the stone (at this t-line)
  ;;> Add option for mouse over view sheet?
  (define (on-mouse me x y)
    (cond [;;> If click on left curl button, set curl to "left"
           ;;> Adjust stone accordingly
           (and (mouse=? me "button-down")
                (< (- (* 1.35 WIDTH-p) (* 1.125 PPF))
                   x
                   (+ (* 1.35 WIDTH-p) (* 1.125 PPF)))
                (< (+ VIEW-HT-p (* .625 SCR-HT-p) (* .75 PPF))
                   y
                   (- (+ VIEW-HT-p (* .625 SCR-HT-p)) (* .75 PPF))))
           (new throw-world%
                (new stone%
                     (this . throw-stone . team)
                     (this . throw-stone . pos)
                     (this . throw-stone . vel)
                     ;;> Extremely rough estimate of starting curl
                     -9
                     315)
                (this . goal-x)
                (this . thrown-stones)
                (this . unthrown-stones)
                "left"
                (this . team1)
                (this . team2)
                (this . end)
                (this . scores))]
          [;;> If click on right curl button, set curl to "right"
           ;;> Adjust stone accordingly
           (and (mouse=? me "button-down")
                (< (- (* 1.65 WIDTH-p) (* 1.125 PPF))
                   x
                   (+ (* 1.65 WIDTH-p) (* 1.125 PPF)))
                (< (+ VIEW-HT-p (* .625 SCR-HT-p) (* .75 PPF))
                   y
                   (- (+ VIEW-HT-p (* .625 SCR-HT-p)) (* .75 PPF))))
           (new throw-world%
                (new stone%
                     (this . throw-stone . team)
                     (this . throw-stone . pos)
                     (this . throw-stone . vel)
                     ;;> Extremely rough estimate of starting curl
                     ;;> based on having to reach straight by release (hog line)
                     9
                     45)
                (this . goal-x)
                (this . thrown-stones)
                (this . unthrown-stones)
                "right"
                (this . team1)
                (this . team2)
                (this . end)
                (this . scores))]
          [;;> If mouse over target sheet...
           (and (< WIDTH-p x (* 2 WIDTH-p))
                (< 0 y VIEW-HT-p))
           (cond
             [(mouse=? me "button-down")
              (new throw-world%
                   (new stone%
                        (this . throw-stone . team)
                        (new posn%
                             (this . throw-stone . pos . x)
                             ;;> Not actually sure what this change should be
                             ;;> Also not sure about the back limit
                             (max (- (this . throw-stone . pos . y) .1)
                                  (- THAT-HACK-POS 1))
                             (this . throw-stone . vel)
                             (this . throw-stone . curl)
                             (this . throw-stone . rot)))
                   (this . goal-x)
                   (this . thrown-stones)
                   (this . unthrown-stones)
                   (this . curl)
                   (this . team1)
                   (this . team2)
                   (this . end)
                   (this . scores))]
             [;;> If release mouse (in this region):
              ;;>    Set velocity (based on distance back from hack/start) and throw (new slide-world% ...)
              (mouse=? me "button-up")
              (new slide-world%
                   (new stone%
                        (this . throw-stone . team)
                        (this . throw-stone . pos)
                        (this . throw-stone . pos . to-from-vel
                              (new posn% (this . goal-x) D2B)
                              ;;> Not certain of scaling (vel should be negative)
                              (* 4 (- (THAT-HACK-POS)
                                      (this . throw-stone . pos . y)))))
                   (this . thrown-stones)
                   (this . unthrown-stones)
                   (this . curl)
                   (this . team1)
                   (this . team2)
                   (this . end)
                   (this . scores))]
             [else
              ;;> Just set the goal position to x
              (new throw-world%
                   (this . throw-stone)
                   (- WIDTH x)
                   (this . thrown-stones)
                   (this . unthrown-stones)
                   (this . curl)
                   (this . team1)
                   (this . team2)
                   (this . end)
                   (this . scores))])]
          [else this]))

  ;; on-key : KeyEvent -> World
  ;; Left and right arrows set curl of stone
  ;;> This might go away (or just be left as alternative to mouse)
  ;;> Would also need to make same adjustments to stone
  #;(define (on-key ke)
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
;; A SlideWorld is:
;;   (new slide-world% Stone Stones Stones String String String Number Scores)
;;   (new slide-world% slide-stone thrown-stones unthrown-stones curl team1 team2 end scores)
;; Where slide-stone is the active sliding stone
(define-class slide-world%
  (super end-world%)
  (fields slide-stone)
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> Move the current stone
  ;;> Check for and act upon any collisions:
  ;;>    Within the set of thrown stones
  ;;>    Between the slide-stone and thrown-stones
  ;;> If all stones (slide-stone and thrown-stones) are stopped
  ;;>    (new throw-world%) , move next stone into position
  ;;> If all stones are thrown
  ;;>    (new between-ends-world%)
  #;(define (on-tick)
      (cond [;;> Check for and act upon any collisions:
             ;;>    Within the set of thrown stones
             ;;>    Between the slide-stone and thrown-stones
             ]
            [;;> If all stones (slide-stone and thrown-stones) are stopped
             ;;>    (new throw-world%) , move next stone into position
             ]
            [;;> If all stones are thrown
             ;;>    (new between-ends-world%)
             ]
            [else ;;> move slide-stone
             (new slide-stone%
                  (this . slide-stone . move)
                  (this . thrown-stones . move-all)
                  (this . unthrown-stones)
                  (this . curl)
                  (this . team1)
                  (this . team2)
                  (this . end)
                  (this . scores))
             ]))
  
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
;; A BetweenEndsWorld is:
;;   (new between-ends-world% Image String String Number Scores)
;;   (new between-ends-world% end-img team1 team2 end Scores)
;; Where end-img is an image of the previous end to draw scores over
(define-class between-ends-world%
  (fields team1 team2 scores end-img)
  
  ;; to-draw : -> Image
  ;;> Draw scores centered on screen
  ;;> Say click/press any key to continue
  (define (to-draw)
    (place-image (text 
                       (* 2 PPF) 'black)
                 (* .25 GAME-HT-p) (* .5 GAME-HT-p)
                 (place-image (text "Press Enter to start a new game"
                                    (* 1.5 PPF) 'black)
                              (overlay (this . scores . draw)
                                       BG))))

  ;; on-tick : -> World
  ;;> Same world (is this necessary?)
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;;> Click to start new end
  
  ;; on-key : KeyEvent -> World
  ;;> Press any key to continue
  )



;; A World for after the game ends
;; A game-over-world is:
;;  (new game-over-world% String String Number Scores)
;;  (new game-over-world% team1 team2 end scores)
(define-class game-over-world%
  (fields team1 team2 scores)
  
  ;; to-draw : -> Image
  ;; Draw scores centered on screen; say who the winner is
  ;; Press enter to start new game
  (define (to-draw)
    (local
      [(define this-winner (this . scores . winner (this . team1) (this . team2)))]
      (place-image (text (if (not (boolean? this-winner))
                             (string-append "Team " this-winner " wins!")
                             "The game is a tie.")
                         (* 2 PPF) 'black)
                   (* .25 GAME-HT-p) (* .5 GAME-HT-p)
                   (place-image (text "Press Enter to start a new game"
                                      (* 1.5 PPF) 'black)
                                (overlay (this . scores . draw)
                                         BG)))))
  
  ;; on-tick : -> World
  ;; Same world (is this necessary?)mber Number -> World
  
  ;; on-key : KeyEvent -> World
  ;;> Start new game if Enter is pressed
  ;;> Not sure if that's the right key
  ;;> Need to have a starting world defined
  #;(define (on-key ke)
    (if (key=? "enter")
        world0
        this))
  
  ;; on-mouse : MouseEvent Number Number -> World
  ;;> Click to start new game?
  )