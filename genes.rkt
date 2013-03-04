#lang class/1

; A Base is one of:
; - Nucleotide
; - NoBase
; And implements
; - complementary? : Base -> Boolean
;   Does the given base pair with this base?
; - pair : Base -> BasePair or false
;   Pair the bases, if they are complementary

; A DNAmol is one of:
; - SingleStrand
; - DoubleStrand

; A Genome is one of
; - EmptyGen
; - ConsGen

; A RestrictionEnzyme is a (new r-enz% [ListofBase] [ListofBase])
; Where rec-site is the sequence required to cut (5' -> 3' direction)
;   and rec-site is palindromic (must pair with itself on other strand)
;   and cut-site is part of rec-site from the beginning to where cut
(define-class r-enz%
  (fields rec-site cut-site))

; Test palindromic property of rec-site


; A SuperBase is a (new super-base%)
(define-class super-base%
  
  ; pair : Base -> BasePair or false
  ; Pair the bases, if they are complementary
  (define (pair b)
    (cond [(this . complementary? b)
           (new bp% this b)]
          [else false]))
  (check-expect (A . pair T) (new bp% A T))
  (check-expect (A . pair MT) (new bp% A MT))
  (check-expect (MT . pair T) (new bp% MT T))
  (check-expect (A . pair G) false))


; An Nucleotide is a (new nuc% String)
; Where base is one of "A", "C", "T", "G"
(define-class nuc%
  (super super-base%)
  (fields base)
  
  ; complementary? : Base -> Boolean
  ; Does the given base pair with this base?
  (define (complementary? n)
    (cond [(n . empty?)
           true]
          [(string=? "A" (this . base))
           (string=? "T" (n . base))]
          [(string=? "T" (this . base))
           (string=? "A" (n . base))]
          [(string=? "C" (this . base))
           (string=? "G" (n . base))]
          [(string=? "G" (this . base))
           (string=? "C" (n . base))]))
          
  (check-expect (A . complementary? T) true)
  (check-expect (T . complementary? A) true)
  (check-expect (C . complementary? G) true)
  (check-expect (G . complementary? C) true)
  (check-expect (A . complementary? T) true)
  (check-expect (A . complementary? G) false)
  (check-expect (G . complementary? G) false)
  (check-expect (T . complementary? G) false)
  (check-expect (C . complementary? T) false)
  (check-expect (A . complementary? MT) true)
  
  ; empty? : -> Boolean
  ; Is the base empty?
  (define (empty?) false)
  (check-expect (T . empty?) false))

; Examples:
(define A (new nuc% "A"))
(define C (new nuc% "C"))
(define T (new nuc% "T"))
(define G (new nuc% "G"))


; A NoBase is a (new no-base%)
(define-class no-base%
  (super super-base%)
  
  ; complementary? : Base -> true
  ; Anything pairs with an empty base
  (define (complementary? n) true)
  (check-expect (MT . complementary? T) true)
  (check-expect (MT . complementary? A) true)
  (check-expect (MT . complementary? C) true)
  (check-expect (MT . complementary? G) true)
  
  ; empty? : -> Boolean
  ; Is the base empty?
  (define (empty?) true)
  (check-expect (MT . empty?) true))

(define MT (new no-base%))


; A BasePair is a (new bp% Base Base)
; Where 5->3 is the base on the 5' -> 3' Strand (reading L to R)
;   and 3->5 is the base on the 3' -> 5' Strand (reading L to R)
(define-class bp%
  (fields 5->3 3->5))

; Examples
(define AT (new bp% A T))
(define TA (new bp% T A))
(define CG (new bp% C G))
(define GC (new bp% G C))

(define -A (new bp% MT A))
(define -T (new bp% MT T))
(define -C (new bp% MT C))
(define -G (new bp% MT G))
(define A- (new bp% A MT))
(define T- (new bp% T MT))
(define C- (new bp% C MT))
(define G- (new bp% G MT))

; A SingleStrand is a (new 1strand% [Listof Base])
; Where Bases are in 5' -> 3' order
(define-class 1strand%
  (fields bases)
  
  ; complementary? : SingleStrand -> Boolean
  ; Do all of the bases on the strands pair?
  ;;> Some way to deal with offest?
  (define (complementary? ss)
    (cond [(and (empty? (this . bases))
                (empty? (ss . bases)))
           true]
          [(or (empty? (this . bases))
                (empty? (ss . bases)))
           false]
          [else
           (and ((first (this . bases)) . complementary? (first (ss . bases)))
                ((new 1strand% (rest (this . bases)))
                 . complementary?
                 (new 1strand% (rest (ss . bases)))))]))
  (check-expect (s1-3 . complementary? s1-3) false)
  (check-expect (s1-2 . complementary? s1-2) true)
  (check-expect (s1-3 . complementary? s1-1) false)
  (check-expect (s1-1 . complementary? s1-4) false)
  (check-expect (s1-1 . complementary? s1-2) false)
  
  ; reverse-strand : -> [Listof Base]
  ; Reverse into 3' -> 5' order (for annealing purposes)
  (define (reverse-strand)
    (reverse (this . bases)))
  (check-expect (s1-1 . reverse-strand) (list G T T C A))
  (check-expect (s1-2 . reverse-strand) empty)
  (check-expect (s1-3 . reverse-strand) (list A T A T))
  
  ; remove-nb : -> SingleStrand
  ; Remove the empty bases from the strand
  (define (remove-nb)
    (new 1strand%
         (filter (λ (b) (not (b . empty?)))
                 (this . bases))))
  
  (check-expect ((new 1strand% (list MT MT C G T)) . remove-nb)
                (new 1strand% (list C G T)))
  (check-expect ((new 1strand% (list C G T MT MT)) . remove-nb)
                (new 1strand% (list C G T)))
  (check-expect ((new 1strand% (list MT C A MT G T)) . remove-nb)
                (new 1strand% (list C A G T)))
  
  ; restrict : RestrictionEnzyme -> Genome or SingleStrand
  ; Cut the Strand into pieces if it has the rec-site
  )

; Examples:
(define s1-1 (new 1strand%
                  (list A C T T G)))
(define s1-2 (new 1strand% empty))
(define s1-3 (new 1strand%
                  (list T A T A)))
(define s1-4 (new 1strand%
                  (list C A A G T)))

; A DoubleStrand is a (new 2strand% [Listof BasePair])
(define-class 2strand%
  (fields bps)
  
  ; ligate : DoubleStrand -> DoubleStrand or false
  ; Turn 2 fragments into 1, if ends match, or return false
  
  ; 5-3-strand : String -> SingleStrand
  ; Get the 5' -> 3' strand out of the double strand
  ; Eliminate empty bases on ends
  (define (5-3-strand)
    (new 1strand%
         (map (λ (bp) (bp . 5->3))
              (this . bps))))
  (check-expect (s2-1 . 5-3-strand)
                (new 1strand% (list C A T G G)))
  (check-expect (s2-2 . 5-3-strand) s1-2)
  (check-expect (s2-3 . 5-3-strand)
                (new 1strand% (list A)))
  
  ; 3-5-strand : String -> SingleStrand
  ; Get the 3' -> 5' strand out of the double strand
  ; Reversed to be presented in 5' -> 3' format (match data definition)
  ; Eliminate empty bases on ends
  (define (3-5-strand)
    (new 1strand%
         ((new 1strand%
               (map (λ (bp) (bp . 3->5))
                    (this . bps))) . reverse-strand)))
  
  (check-expect (s2-1 . 3-5-strand)
                (new 1strand% (list C C A T G)))
  (check-expect (s2-2 . 3-5-strand) s1-2)
  (check-expect (s2-3 . 3-5-strand)
                (new 1strand% (list T)))
  
  ; anneal : -> Genome
  ; Split the DoubleStrand into 2 SingleStrands
  ; (list 5->3 3->5)
  (define (anneal)
      (list (this . 5-3-strand)
            (this . 3-5-strand)))
  
  (check-expect (s2-1 . anneal)
                (list (new 1strand% (list C A T G G))
                      (new 1strand% (list C C A T G))))
  (check-expect (s2-2 . anneal)
                (list (new 1strand% empty)
                      (new 1strand% empty)))
    
  ; 3-oh-lead : -> SingleStrand
  ; Give the 3' overhang of the leading end of the double strand
  ; Returns empty if blunt ends
  (define (3-oh-lead)
    (local [(define (base-list bps)
              (cond [(empty? bps)
                     empty]
                    [((first bps) . 5->3 . empty?)
                     (cons ((first bps) . 3->5)
                           (base-list (rest bps)))]
                    [else empty]))]
      (new 1strand% (base-list (this . bps)))))
  
  (check-expect (s2-1 . 3-oh-lead)
                (new 1strand% empty))
  (check-expect (s2-3ohl . 3-oh-lead)
                (new 1strand% (list A A T G)))
  (check-expect (s2-5ohl . 3-oh-lead)
                (new 1strand% empty))
  (check-expect (s2-3ohl . 3-oh-lead)
                (new 1strand% (list A A T G)))
  
  ; 3-oh-tail : -> SingleStrand
  ; Give the 3' overhang of end of the double strand
  ; Returns empty if blunt ends
  #;(define (3-oh-tail)
    (cond [(empty? (this . bps))
           (new 1strand% empty)]
          []))
  
  ; overhang-5' : -> SingleStrand
  ; Give the 5' overhang of the fragment, if any
  
  ; restrict : RestrictionEnzyme -> Genome or DoubleStrand
  ; Cut the DoubleStrand according to the enzyme as many times as necessary
  ; Return this if cut-site not present
  )

; Examples:
(define s2-1 (new 2strand%
                (list CG AT TA GC GC)))
(define s2-2 (new 2strand% empty))
(define s2-3 (new 2strand%
                  (list AT)))
(define s2-4 (new 2strand%
                  (list AT TA)))
(define s2-3ohl (new 2strand%
                    (list -A -A -T -G AT CG TA)))
(define s2-5ohl (new 2strand%
                    (list A- C- G- GC TA)))
(define s2-3oht (new 2strand%
                     (list AT CG A- T-)))
(define s2-5oht (new 2strand%
                     (list TA AT GC -T -T -T)))


; A SuperGenome is a (new super-gen%)
(define-class super-gen%
    
  ; cons : DNAmol -> Genome
  ; Add the DNAmol to the Genome
  (define (cons dna)
    (new consgen% dna (new emptygen%))))

; An EmptyGen is a (new emptygen%)
(define-class emptygen%
  (super super-gen%)
  
  ; empty? : -> Boolean
  ; Is the Genome empty? (yes)
  (define (empty?) true))


; A ConsGen is a (new consgen% DNAmol Genome)
(define-class consgen%
  (super super-gen%)
  (fields first rest)
  
  ; empty? : -> Boolean
  ; Is the Genome empty? (no)
  )



