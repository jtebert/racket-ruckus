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


#|-------------------------------------|
 |          REGION DETECTION           |
 |-------------------------------------|#

;; mouse-on-sheet : Number Number Number -> [ or Posn false]
;; Given the mouse position in the scene (x,y)
;;    and the offest of the view (left) sheet in pixels
;; gives the position of the mouse on the sheet based on game layout (in meters)
;; If not over the sheet, return false
(define (mouse-on-sheet x y view-pos)
  (cond [;; mouse not over any sheet
         (not (and (< x (* 2 WIDTH-p))
                   (< y VIEW-HT-p)))
         false]
        [;; mouse over view sheet
         ;;> Not sure about use of that adjusted value
         (< x WIDTH-p)
         (new posn% (/ x PPM) (/ (- y view-pos) PPM))]
        [;; mouse over target sheet
         ;;> Not sure if y adjustment is correct
         else
         (new posn% (- (/ x PPM) WIDTH)
                    (+ (/ y PPM) (- THIS-BACKLINE-POS (* 2 STONE-R))))]))
(check-expect (mouse-on-sheet 600 600 0) false)
(check-expect (mouse-on-sheet 300 600 0) false)
(check-expect (mouse-on-sheet 600 300 0) false)
(check-expect (mouse-on-sheet 130 130 0)
              (new posn% 2 2))
(check-expect (mouse-on-sheet 130 130 65)
              (new posn% 2 1))
(check-within (mouse-on-sheet 325 130 0)
              (new posn% (- 5 WIDTH) (+ 2 (- THIS-BACKLINE-POS (* 2 STONE-R))))
              .0001)

#|-------------------------------------|
 |               WORLDS                |
 |-------------------------------------|#

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
  (fields team1 team2 end scores debug*))


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
  (super s-world%)
  (fields thrown-stones unthrown-stones curl)
  
  ;; draw : Image Number Image -> Image
  ;; Takes in the image of the SHEET to be drawn (including stones, etc.)
  ;;   and the crop coordinate for the view-sheet
  ;;   and an image for debugging to place in the upper left corner
  ;; Current stone centered left window
  ;; Scoreboard at the bottom
  ;; House on the left
  ;; Overview of sheet on far left
  ;;> Need velocity indicator
  ;;> Drawing broom in slide-world?
  (define (draw sheet/stones view-offset debug*)
    (local
      [(define scaled-sheet (frame (scale SCALE sheet/stones)))
       (define target-sheet
         (frame (crop 0 (- THIS-BACKLINE-POS-p (* 2 STONE-R-p))
                      WIDTH-p VIEW-HT-p
                      sheet/stones)))
       (define view-sheet
         (frame (crop 0 view-offset
                      WIDTH-p VIEW-HT-p
                      sheet/stones)))
       (define scores-img
         (frame (this . scores . draw (this . team1) (this . team2))))]
       (place-image
        debug*
        100 50
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
            BG)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A World in which the player is throwing a Stone
;; A ThrowWorld is:
;;   (new throw-world% Boolean      Stone       Number Stones        Stones          String String String Number Scores)
;;   (new throw-world% button-down? throw-stone goal-x thrown-stones unthrown-stones curl   team1  team2  end    scores)
;; Where throw-stone is the stone being thrown
;;   and goal-x is the x position at the T of the far house the stone will go to
(define-class throw-world%
  (super end-world%)
  (fields button-down? throw-stone goal-x)
  
  (define (tick-rate) .5)
  
  ;; to-draw : -> Image
  ;; Draw the whole game window (using abstracted draw function)
  ;;> Added (untested) line from hack to goal-x on t-line
  ;;> Need to add drawing broom (will likely have to save mouse position in world)
  (define (to-draw)
    (local
      [(define sheet/stones
         (add-line (this . thrown-stones . draw-all (this . team1)
                         (this . unthrown-stones . draw-all (this . team1)
                               (this . throw-stone . draw (this . team1) SHEET-IMG)))
                   (/ WIDTH-p 2) THAT-HACK-POS-p
                   (* (this . goal-x) PPM) THIS-TLINE-POS-p
                   'green))
       (define view-offset (- LENGTH-p VIEW-HT-p))
       (define debug-txt
         (above(text (string-append "view-offset(px): "
                                    (number->string (round view-offset)))
                     12 "black")
               (text (string-append "mouse (px): ("
                                     (number->string (round (this . debug* . x)))
                                     ","
                                     (number->string (round (this . debug* . y)))
                                     ")")
                     12 "black")))]
      (this . draw 
            sheet/stones
            view-offset
            debug-txt)))
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> If stone being dragged, move it back
  ;;>>> I think that's taken care of by on-mouse? Need to test/try
  (define (on-tick)
    (if (this . button-down?)
        (new throw-world%
             true
             (new stone%
                  (this . throw-stone . team)
                  (new posn%
                       (this . throw-stone . pos . x)
                       ;;> Not actually sure what this change should be
                       ;;> Also not sure about the back limit
                       (min (+ (this . throw-stone . pos . y) .05)
                            (+ THAT-HACK-POS 1)))
                  (this . throw-stone . vel)
                  (this . throw-stone . curl)
                  (this . throw-stone . rot))
             (this . goal-x)
             (this . thrown-stones)
             (this . unthrown-stones)
             (this . curl)
             (this . team1)
             (this . team2)
             (this . end)
             (this . scores)
             (this . debug*))
        this))
  
  ;; on-mouse : Number Number MouseEvent -> World
  ;; Mouse x position is the target of the stone (at this t-line)
  (define (on-mouse x y me)
    (local [(define adj-mouse-posn (mouse-on-sheet x y (- LENGTH-p VIEW-HT-p)))]
      (cond [;; If mouse over target or view sheet...
             (not (boolean? adj-mouse-posn))
             (cond [;; If button clicked...
                    ;; Drag back stone
                    (mouse=? me "button-down")
                    (new throw-world%
                         true
                         (this . throw-stone)
                         (this . goal-x)
                         (this . thrown-stones)
                         (this . unthrown-stones)
                         (this . curl)
                         (this . team1)
                         (this . team2)
                         (this . end)
                         (this . scores)
                         (new posn% x y))]
                   [;; If release mouse...
                    ;; Set velocity (based on distance back from hack/start) and throw (new slide-world% ...)
                    (mouse=? me "button-up")
                    (new slide-world%
                         (new stone%
                              (this . throw-stone . team)
                              (this . throw-stone . pos)
                              (this . throw-stone . pos . to-from-vel
                                    (new posn% (this . goal-x) D2B)
                                    ;;> Not certain of scaling (vel should be negative)
                                    (* -4 (- THAT-HACK-POS
                                             (this . throw-stone . pos . y))))
                              (this . throw-stone . curl)
                              (this . throw-stone . rot))
                         (this . thrown-stones)
                         (this . unthrown-stones)
                         (this . curl)
                         (this . team1)
                         (this . team2)
                         (this . end)
                         (this . scores)
                         (new posn% x y))]
                   [else
                    ;; Just set the goal position to x
                    (new throw-world%
                         (this . button-down?)
                         (this . throw-stone)
                         ;; Uses current x and y to find straight path to goal y on t-line
                         (+ (/ WIDTH 2)
                            (/ (* (- (adj-mouse-posn . x) (/ WIDTH 2)) (- THIS-TLINE-POS THAT-HACK-POS))
                               (- (adj-mouse-posn . x) THAT-HACK-POS)))
                         (this . thrown-stones)
                         (this . unthrown-stones)
                         (this . curl)
                         (this . team1)
                         (this . team2)
                         (this . end)
                         (this . scores)
                         (new posn% x y))])]
            [;; If click on left curl button, set curl to "left"
             ;; Adjust stone accordingly
             (and (mouse=? me "button-down")
                  (< (- (* 1.65 WIDTH-p) (* 1.125 PPF))
                     x
                     (+ (* 1.65 WIDTH-p) (* 1.125 PPF)))
                  (< (- (+ VIEW-HT-p (* .625 SCR-HT-p)) (* .75 PPF))
                     y
                     (+ VIEW-HT-p (* .625 SCR-HT-p) (* .75 PPF))))
             (new throw-world%
                  (this . button-down?)
                  (new stone%
                       (this . throw-stone . team)
                       (this . throw-stone . pos)
                       (this . throw-stone . vel)
                       ;;> Extremely rough estimate of starting curl
                       -9
                       -45)
                  (this . goal-x)
                  (this . thrown-stones)
                  (this . unthrown-stones)
                  "left"
                  (this . team1)
                  (this . team2)
                  (this . end)
                  (this . scores)
                  (new posn% x y))]
            [;; If click on right curl button, set curl to "right"
             ;; Adjust stone accordingly
             (and (mouse=? me "button-down")
                  (< (- (* 1.85 WIDTH-p) (* 1.125 PPF))
                     x
                     (+ (* 1.85 WIDTH-p) (* 1.125 PPF)))
                  (< (- (+ VIEW-HT-p (* .625 SCR-HT-p)) (* .75 PPF))
                     y
                     (+ VIEW-HT-p (* .625 SCR-HT-p) (* .75 PPF))))
             (new throw-world%
                  (this . button-down?)
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
                  (this . scores)
                  (new posn% x y))]
            [else
             ;; If not setting curl or over sheets...
             ;; Return same world (just change debug*)
             #;this
             (new throw-world%
                  (this . button-down?)
                  (this . throw-stone)
                  (this . goal-x)
                  (this . thrown-stones)
                  (this . unthrown-stones)
                  (this . curl)
                  (this . team1)
                  (this . team2)
                  (this . end)
                  (this . scores)
                  (new posn% x y))])))

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

;; world-start : String String -> World
;; Create an initial World with the given team names
(define (world-start team1 team2)
  (local [(define start-stones (stones-start team1 team2))
          (define first-stone (first (start-stones . stones)))]
    (new throw-world%
         false
         (new stone%
              (first-stone . team)
              (new posn% (/ WIDTH 2) THAT-HACK-POS)
              (first-stone . vel)
              ;;> Curl and rotation will probably need to actually
              ;;> be set based on world state curl
              -9 -45)
         (/ WIDTH 2) ;;> Not sure about goal
         (new stones% empty)
         (new stones% (rest (start-stones . stones)))
         ;;> default starting with "left"?
         "left" team1 team2 1
         (new scores% empty)
         (new posn% 0 0))))
#;(check-within (world-start "A" "B")
              (new throw-world%
                   false
                   (first ((stones-start "A" "B") . stones))
                   (/ WIDTH 2)
                   (new stones% empty)
                   (new stones% (rest ((stones-start "A" "B") . stones)))
                   "left" "A" "B" 1
                   (new scores% empty)
                   (new posn% 0 0))
              .1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A World in which player is directing thrown stone
;; A SlideWorld is:
;;   (new slide-world% Stone       Stones        Stones          String String String Number Scores)
;;   (new slide-world% slide-stone thrown-stones unthrown-stones curl   team1  team2  end    scores)
;; Where slide-stone is the active sliding stone
(define-class slide-world%
  (super end-world%)
  (fields slide-stone)
  
  ;; on-tick : -> World
  ;; Produce the next world state
  ;;> Probably redundancy in filtering
  (define (on-tick)
    (local [(define all-stones
              (new stones% (cons (this . slide-stone) (this . thrown-stones . stones))))
            (define next-stone
              (first (this . unthrown-stones . stones)))
            ;; filter-in-play : Stones -> Stones
            (define (filter-in-play stns)
              (new stones% (filter (λ (s) (not (or (s . off-sheet?)
                                                   (s . hogged?))))
                                   (stns . stones))))]
      (cond [;; If any collisions...
             (all-stones . any-collisions?)
             ;; Act on them, move the stones, filter out of play stones
             (local [(define col-stones
                       (all-stones . collide-all . stones))]
               (new slide-world%
                    ((first col-stones) . move)
                    (filter-in-play ((new stones% (rest col-stones)) . move-all))
                    (this . unthrown-stones)
                    (this . curl)
                    (this . team1)
                    (this . team2)
                    (this . end)
                    (this . scores)
                    (this . debug*)))]
            [;; If all stones are stopped or out of play...
             (and (andmap (λ (s) (s . stopped?))
                          (this . thrown-stones . stones))
                  (or (this . slide-stone . stopped?)
                      (this . slide-stone . hogged?)
                      (this . slide-stone . off-sheet?)))
             ;; If all stones are thrown...
             (if (not (empty? (this . unthrown-stones . stones)))
                 ;; (new throw-world%) , move next stone into position
                 (local [(define next-stone
                           (first (this . unthrown-stones . stones)))]
                   (new throw-world%
                        false
                        (new stone%
                             (next-stone . team)
                             (new posn% (/ WIDTH 2) THAT-HACK-POS)
                             (next-stone . vel)
                             ;;> Curl and rotation will probably need to actually
                             ;;> be set based on world state curl
                             (next-stone . curl)
                             (next-stone . rot))
                        (/ WIDTH 2) ;;> temporarily set goal-x to middle
                        (filter-in-play all-stones)
                        (new stones% (rest (this . unthrown-stones . stones)))
                        (this . curl) ;; keep same curl as previous time
                        (this . team1)
                        (this . team2)
                        (this . end)
                        (this . scores)
                        (this . debug*)))
                 ;;> else (new between-ends-world%)
                 (new between-ends-world%
                      (this . to-draw)
                      (this . team1)
                      (this . team2)
                      (this . end)
                      (new scores%
                           (cons (this . thrown-stones . end-score (this . end))
                                 (this . scores . scores)))))]
            [else ;;> move slide-stone
             (new slide-world%
                  (this . slide-stone . move)
                  (filter-in-play (this . thrown-stones . move-all))
                  (this . unthrown-stones)
                  (this . curl)
                  (this . team1)
                  (this . team2)
                  (this . end)
                  (this . scores)
                  (this . debug*))])))
  
  ;; to-draw : -> Image
  ;; Draw the sheet and stuff
  (define (to-draw)
    (local
      [(define sheet/stones
         (this . thrown-stones . draw-all (this . team1)
                         (this . unthrown-stones . draw-all (this . team1)
                               (this . slide-stone . draw (this . team1) SHEET-IMG))))]
      (this . draw
            sheet/stones
            (min (max (- THIS-BACKLINE-POS-p (* 2 STONE-R-p))
                      (- (* PPM (this . slide-stone . pos . y))
                         (/ VIEW-HT-p 2)))
                 (- LENGTH-p VIEW-HT-p))
            empty-image)))
  
  ;; on-mouse : Number Number MouseEvent -> World
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
                 (place-image (text "Press any key to start a new end"
                                    (* 1.5 PPF) 'black)
                              (overlay (this . scores . draw)
                                       BG))))

  ;; on-tick : -> World
  ;;> Same world (is this necessary?)
  (define (on-tick) this)
  
  ;; on-mouse : Number Number MouseEvent -> World
  ;;> Click to start new end
  
  ;; on-key : KeyEvent -> World
  ;; Press any key to continue
  (define (on-key ke)
    (new-end))
  
  ;; new-end : -> World
  ;; Starts a new end with a new ThrowWorld
  (define (new-end)
    (local [(define hammer ((first (this . scores . scores)) . team))
            (define new-stones
              (stones-start
               (if (string=? (this . team1) hammer)
                   (this . team2) (this . team1))
               hammer))
            (define first-stone (first (new-stones . stones)))]
    (new throw-world%
         false
         (new stone%
              (first-stone . team)
              (new posn% (/ WIDTH 2) THAT-HACK-POS)
              (first-stone . vel)
              ;;> Curl and rotation will probably need to actually
              ;;> be set based on world state curl
              (first-stone . curl)
              (first-stone . rot))
         ;;> Not sure about goal
         (/ WIDTH 2)
         (new stones% empty)
         (new stones% (rest (new-stones . stones)))
         (this . curl) ;;> Not sure about this either
         (this . team1)
         (this . team2)
         (add1 (this . end))
         (this . scores)
         (this . debug*)))))



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
  ;; Same world (is this necessary?)
  (define (on-tick) this)
  
  ;; on-key : KeyEvent -> World
  ;; Start new game if Enter is pressed
  (define (on-key ke)
    (if (key=? "\r")
        (world-start (this . team1) (this . team2))
        this))
  
  ;; on-mouse : Number Number MouseEvent -> World
  ;;> Click to start new game?
  )

(define (start team1 team2)
  (big-bang (world-start team1 team2)))