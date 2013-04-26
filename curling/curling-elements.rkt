#lang class/2

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
  ;;> Needs change due to curl
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
  ;;> Tested by visual inspection
  
  ;; move-all : -> Stones
  ;; Move all of the stones in the set
  ;;> Untested until movement with curl implemented
  (define (move-all)
    (new stones% (map (λ (s )(s . move)) (this . stones))))
  (check-expect (stones0 . move-all) stones0)
  
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