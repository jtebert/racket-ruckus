#lang class/1

(require 2htdp/image class/universe)

; Constants
(define TEXT-SIZE 36)
(define W 700)
(define H 80)
(define BG (empty-scene W H))

; A Message is one of:
; - (list Symbol String)
;   Where Symbol is the type of Message ('name or 'password)
;     and String is the content of the Message
; - Boolean

; A Client is one of:
; - Password-client
; - Name-client


; start-client : -> Client
; Open a new Client, starting as a Password-client
(define (start-client)
  (big-bang pc0))

#;(define (start password)
  (launch-many-worlds
   (universe (new server% password empty))
   (big-bang pc0)))

; type : KeyEvent String -> String
; Type the letter or delete
(define (type ke s)
  (cond [(and (key=? ke "\b")
              (not (= 0 (string-length s))))
         (substring s 0 (sub1 (string-length s)))]
        [(and (not (key=? ke "\b"))
              (= 1 (string-length ke)))
         (string-append s ke)]
        [else s]))

(check-expect (type "\b" "a") "")
(check-expect (type "\b" "") "")
(check-expect (type "up" "a") "a")
(check-expect (type "a" "H") "Ha")

; draw-text : String Symbol -> Color
; Draw String in the given color
(define (draw-text str color)
  (local [(define txt (text str TEXT-SIZE color))]
    (place-image txt
                 (+ 10 (/ (image-width txt) 2))
                 (/ H 2)
                 BG)))
(check-expect (draw-text "Hello World" 'blue)
              (place-image (text "Hello World" TEXT-SIZE 'blue)
                           (+ 10 (/ (image-width (text "Hello World" TEXT-SIZE 'blue))
                                    2))
                           (/ H 2)
                           BG))


; A Password-client is a (new password-client% String)
; Client in which the user is guessing the password
(define-class password-client%
  (fields guess)
  
  ; register : -> String
  ; Register the Name-client with the host (LOCALHOST)
  (define (register) LOCALHOST)
  
  ; on-key : String -> Name-client
  ; Create the name-client's name as it is typed
  ; Enter sends name to server
  ; If name is changed, resets sent? to #f
  (define (on-key ke)
    (cond [(key=? ke "\r")
           (make-package this (list 'password (this . guess)))]
          [else (new password-client%
                     (type ke (this . guess)))]))
  (check-expect (pc1 . on-key "\r")
                (make-package pc1 (list 'password "123")))
  (check-expect (pc1 . on-key "\b")
                (new password-client% "12"))
  
  ; on-receive : Message -> Client
  ; If the password guess was correct, go to name server
  ; If not reset guess field
  (define (on-receive msg)
    (cond [(and (boolean? msg) msg)
           (new name-client% "" false)]
          [(boolean? msg)
           (new password-client% "")]
          [else this]))
  (check-expect (pc1 . on-receive true) (new name-client% "" false))
  (check-expect (pc1 . on-receive false) (new password-client% ""))
  (check-expect (pc1 . on-receive "false") pc1)
  
  ; to-draw : -> Image
  ; Draw the name on the scene, black if unsent, green if sent
  (define (to-draw)
    (draw-text (string-append "Enter password: "
                              (replicate (string-length (this . guess)) "*"))
               'red))
  (check-expect (pc1 . to-draw)
                (place-image (text "Enter password: ***" TEXT-SIZE 'red)
                           (+ 10 (/ (image-width
                                     (text "Enter password: ***"
                                           TEXT-SIZE 'red)) 2))
                           (/ H 2)
                           BG)))

; Examples:
(define pc0 (new password-client% ""))
(define pc1 (new password-client% "123"))


; A Name-client is a (new name-client% String Boolean)
; Boolean indicates if the name has been submitted
(define-class name-client%
  (fields name sent?)
  
  ; on-key : String -> Client
  ; Create the name-client's name as it is typed
  ; Enter sends name to server
  ; If name is changed, resets sent? to #f
  (define (on-key ke)
    (local [(define typed (type ke (this . name)))]
      (cond [(key=? ke "\r")
             (make-package (new name-client% (this . name) true)
                           (list 'name (this . name)))]
            [else (new name-client%
                       typed
                       (and (this . sent?)
                            (string=? (this . name) typed)))])))
  (check-expect (c1 . on-key " ")
                (new name-client% " " #f))
  (check-expect (c2 . on-key "\r")
                (make-package (new name-client% "Voldemort" #t)
                              (list 'name "Voldemort")))
  (check-expect (c2 . on-key "\b")
                (new name-client% "Voldemor" #f))
  (check-expect (c3 . on-key " ")
                (new name-client% "Harry " #f))
  (check-expect (c3 . on-key "up")
                (new name-client% "Harry" #t))
  (check-expect (c1 . on-key "\b") c1)
  
  ; to-draw : -> Image
  ; Draw the name on the scene, black if unsent, green if sent
  (define (to-draw)
    (draw-text (string-append "Name: " (this . name))
               (cond [(this . sent?) "green"]
                     [else "black"])))
  (check-expect (c1 . to-draw)
                (place-image (text "Name: " TEXT-SIZE "black")
                             (+ 10 (/ (image-width
                                       (text "Name: " TEXT-SIZE "black")) 2))
                             (/ H 2)
                             BG))
  (check-expect (c2 . to-draw)
                (place-image (text "Name: Voldemort" TEXT-SIZE "black")
                             (+ 10 (/ (image-width
                                       (text "Name: Voldemort"
                                             TEXT-SIZE "black")) 2))
                             (/ H 2)
                             BG))
  (check-expect (c3 . to-draw)
                (place-image (text "Name: Harry" TEXT-SIZE "green")
                             (+ 10 (/ (image-width
                                       (text "Name: Harry"
                                             TEXT-SIZE "green")) 2))
                             (/ H 2)
                             BG)))

; Examples:
(define c1 (new name-client% "" #f))
(define c2 (new name-client% "Voldemort" #f))
(define c3 (new name-client% "Harry" #t))