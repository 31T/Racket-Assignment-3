;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;**************************
;;  Brendan Zhang (20720995)
;;  CS 135 Fall 2017
;;  Assignment 03, Problem 4  
;;**************************
;;


(define-struct card (strength colour))
;;A Card is a (make-card Nat Sym)
;; requires: 1 <= strength <= 9
;;           colour is one of: 'red 'yellow 'green 'blue 'purple 'brown

(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)


;;(colour? hand) consumes a Hand and outputs whether the Hand has three
;;  Cards of the same colour or not
;;colour?: Hand -> Bool

;;Examples:
(check-expect (colour? (make-hand (make-card 2 'blue)
                                  (make-card 3 'blue)
                                  (make-card 4 'blue))) true)

(define (colour? hand)
  (cond
    [(and (symbol=? (card-colour (hand-c1 hand))
                    (card-colour (hand-c2 hand)))
          (symbol=? (card-colour (hand-c1 hand))
                    (card-colour (hand-c3 hand))))
     true]
    [else false]))


;;(three-of-a-kind? hand) consumes a Hand and outputs whether the Hand has three
;;   Cards with the same strength or not
;;three-of-a-kind?: Hand -> Bool

;;Examples:
(check-expect (three-of-a-kind? (make-hand (make-card 2 'blue)
                                  (make-card 2 'red)
                                  (make-card 2 'blue))) true)

(define (three-of-a-kind? hand)
  (cond
    [(= (card-strength (hand-c1 hand))
        (card-strength (hand-c2 hand))
        (card-strength (hand-c3 hand)))
     true]
    [else false]))


;;(run? hand) consumes a Hand and outputs whether the Hand has three Cards
;;  with consecutively numbered strengths
;;run?: Hand -> Bool

;;Examples:
(check-expect (run? (make-hand (make-card 2 'blue)
                               (make-card 3 'blue)
                               (make-card 4 'blue))) true)

(define (run? hand)
  (cond
    [(and (= (+ 2 (min (card-strength (hand-c1 hand))
                       (card-strength (hand-c2 hand))
                       (card-strength (hand-c3 hand))))
             (max (card-strength (hand-c1 hand))
                  (card-strength (hand-c2 hand))
                  (card-strength (hand-c3 hand))))
          (or (= (card-strength (hand-c3 hand))
                 (+ 1 (min (card-strength (hand-c1 hand))
                           (card-strength (hand-c2 hand))
                           (card-strength (hand-c3 hand)))))
              (= (card-strength (hand-c2 hand))
                 (+ 1 (min (card-strength (hand-c1 hand))
                           (card-strength (hand-c2 hand))
                           (card-strength (hand-c3 hand)))))
              (= (card-strength (hand-c1 hand))
                 (+ 1 (min (card-strength (hand-c1 hand))
                           (card-strength (hand-c2 hand))
                           (card-strength (hand-c3 hand)))))))
     true]
    [else false]))


;;(sum-of-cards hand) consumes a Hand and outputs the sum of the strengths
;;  of the three Cards
;;sum-of-cards: Hand -> Nat

;;Examples
(check-expect (sum-of-cards (make-hand (make-card 2 'blue)
                                       (make-card 3 'blue)
                                       (make-card 4 'blue))) 9)

(define (sum-of-cards hand)
  (+ (card-strength (hand-c1 hand))
     (card-strength (hand-c2 hand))
     (card-strength (hand-c3 hand))))


;;(battle hand1 hand2) consumes two Hands (hand1 and hand2) and determines
;;  which Hand and their respective player wins as per the conditions
;;  of the game Schotten Totten
;;battle: Hand Hand -> Sym

;;Examples:
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))) 'player1)

(define (battle hand1 hand2)
  (cond
    [(and (and (run? hand1) (colour? hand1))
          (not(and (colour? hand2) (run? hand2)))) 'player1]
    [(and (run? hand1) (colour? hand1))
     (cond
       [(>= (sum-of-cards hand1) (sum-of-cards hand2)) 'player1]
       [else 'player2])]
    [(and (run? hand2) (colour? hand2)) 'player2]
    [(and (three-of-a-kind? hand1) (not(three-of-a-kind? hand2))) 'player1]
    [(three-of-a-kind? hand1)
     (cond
       [(>= (sum-of-cards hand1) (sum-of-cards hand2)) 'player1]
       [else 'player2])]
    [(three-of-a-kind? hand2) 'player2]
    [(and (colour? hand1) (not(colour? hand2))) 'player1]
    [(colour? hand1)
     (cond
       [(>= (sum-of-cards hand1) (sum-of-cards hand2)) 'player1]
       [else 'player2])]
    [(colour? hand2) 'player2]
    [(and (run? hand1) (not(run? hand2))) 'player1]
    [(run? hand1)
     (cond
       [(>= (sum-of-cards hand1) (sum-of-cards hand2)) 'player1]
       [else 'player2])]
    [(run? hand2) 'player2]
    [else
     (cond
       [(>= (sum-of-cards hand1) (sum-of-cards hand2)) 'player1]
       [else 'player2])]))

;;Tests:
(check-expect (battle (make-hand (make-card 4 'blue)
                                 (make-card 3 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 2 'blue)
                                 (make-card 2 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 2 'blue)
                                 (make-card 3 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 1 'blue)
                                 (make-card 2 'blue)
                                 (make-card 3 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 2 'blue)
                                 (make-card 3 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'red)
                                 (make-card 3 'blue)
                                 (make-card 2 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 4 'red)
                                 (make-card 4 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'red)
                                 (make-card 2 'blue)
                                 (make-card 2 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 4 'red)
                                 (make-card 4 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'red)
                                 (make-card 2 'blue)
                                 (make-card 2 'blue))
                      (make-hand (make-card 4 'blue)
                                 (make-card 4 'red)
                                 (make-card 4 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'red)
                                 (make-card 2 'blue)
                                 (make-card 2 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 1 'red)
                                 (make-card 1 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 6 'blue)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 6 'blue)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 4 'red)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 5 'blue)
                                 (make-card 5 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 8 'red)
                                 (make-card 7 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 4 'blue)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 7 'blue)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 4 'red)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 5 'blue)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'red)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 8 'red)
                                 (make-card 7 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 6 'blue)
                                 (make-card 8 'red)
                                 (make-card 9 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 8 'red)
                                 (make-card 7 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'red)
                                 (make-card 5 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 6 'red)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'red)
                                 (make-card 4 'blue))
                      (make-hand (make-card 5 'blue)
                                 (make-card 6 'red)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 2 'blue)
                                 (make-card 3 'red)
                                 (make-card 4 'blue))
                      (make-hand (make-card 1 'blue)
                                 (make-card 2 'red)
                                 (make-card 3 'blue))) 'player1)



       
   