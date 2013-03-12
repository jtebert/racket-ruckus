#lang class/1

; binary-rep : Number -> Number
; Creates a binary number of n bits
(define (binary-rep n)
  (cond [(<= n 0) 0]
        [else (+ (* (expt 10 (sub1 n)) (random 2))
                 (binary-rep (sub1 n)))]))

; CREATE RANDOMIZED TESTING
(check-expect (<= (string-length (number->string (binary-rep 1000)))
                 1000) true)
(check-expect (<= (string-length (number->string (binary-rep 2)))
                 2) true)