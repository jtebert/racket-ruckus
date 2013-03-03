#lang class/1

(require class/universe)

; To take attendance and see who's missing, run:
; ((start-server password) . absent roster)
; Where password is the log-in password for the session
;   and roster is a list of the names of the students

; A Message is one of:
; - (list Symbol String)
;   Where Symbol is the type of Message ('name or 'password)
;     and String is the content of the Message
; - Boolean


; start-server : String -> Server
; Start the Server with the given password
(define (start-server password)
  (universe (new server% password empty)))

; list-msg? : Message -> Boolean
; Is the Message of the form (list Symbol String)
(define (list-msg? msg)
  (and (cons? msg)
       (= 2 (length msg))
       (symbol? (first msg))
       (string? (second msg))))
(check-expect (list-msg? true) false)
(check-expect (list-msg? (list 'correct "message")) true)
(check-expect (list-msg? (list "bad" "message")) false)

; contains-name? String [Listof String] -> Boolean
; Does the list of Names contain the given name?
(define (contains-name? s los)
  (ormap (λ (name) (string=? s name))
         los))
(check-expect (contains-name? "a" '("a" "b" "c")) true)
(check-expect (contains-name? "d" '("a" "b" "c")) false)
(check-expect (contains-name? "a" empty) false)


; A Server is a (new server% [Listof String])
(define-class server%
  (fields password names)
  
  ; on-msg : IWorld Message -> Server
  ; If sending password: tell Client if it's correct
  ; If sending name: add msg to the list of names if it's not already there
  (define (on-msg iw msg)
    (local [(define (contains? l s)
              (ormap (λ (ls) (string=? ls s))
                     l))]
      (cond [(list-msg? msg)
             (cond [(symbol=? (first msg) 'password)
                    (make-bundle this
                                 (list (make-mail iw
                                                  (string=? (second msg)
                                                            (this . password))))
                                 empty)]
                   [(contains? (this . names) (second msg))
                    this]
                   [else
                    (new server% (this . password)
                         (cons (second msg) (this . names)))])]
            [else this])))
  (check-expect (s1 . on-msg iworld1 (list 'name "Ron"))
                (new server% "password" (list "Ron")))
  (check-expect (s2 . on-msg iworld1 (list 'name "Hermione"))
                (new server% "123" (list "Hermione" "Harry" "Ron")))
  (check-expect (s2 . on-msg iworld1 (list 'name "Ron")) s2)
  (check-expect (s1 . on-msg iworld1 (list 'password "open sesame"))
                (make-bundle s1 (list (make-mail iworld1 false)) empty))
  (check-expect (s2 . on-msg iworld1 (list 'password "123"))
                (make-bundle s2 (list (make-mail iworld1 true)) empty))
  
  ; here : [Listof String] -> [Listof String]
  ; Given the roster, return the list of those in the server present
  (define (here roster)
    (filter (λ (name) (contains-name? name roster))
            (this . names)))
  (check-expect (s1 . here roster) empty)
  (check-expect (s2 . here roster) '("Harry"))
  
  ; absent : [Listof String] -> [Listof String]
  ; List those on the roster not in the server
  (define (absent roster)
    (filter (λ (name) (not (contains-name? name (this . names))))
            roster))
  (check-expect (s1 . absent roster) roster)
  (check-expect (s2 . absent roster)
                '("Hermione" "Malfoy" "Neville" "Cedric")))

; Examples:
(define s1 (new server% "password" empty))
(define s2 (new server% "123" (list "Harry" "Ron")))

(define roster '("Harry" "Hermione" "Malfoy" "Neville" "Cedric"))