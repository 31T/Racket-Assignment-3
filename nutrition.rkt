;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;**************************
;;  Brendan Zhang (20720995)
;;  CS 135 Fall 2017
;;  Assignment 03, Problem 2  
;;**************************
;;


(define-struct nutri-fact (name serving fat carbs sugar protein))
;;A Nutri-Fact is a (make-nutri-fact Str Num Num Num Num Num)
;; requires: 0 < serving
;;          fat + carbs + protein <= serving
;;          0 <= sugar <= carbs
;;          0 <= fat, protein


;;Part A

;;my-nutri-fact-fn: Nutri-Fact -> Any
(define (my-nutri-fact-fn fact)
  (... (nutri-fact-name)...
       (nutri-fact-serving)...
       (nutri-fact-fat)...
       (nutri-fact-carbs)...
       (nutri-fact-sugar)...
       (nutri-fact-protein)...))


;;Part B

;;(resize nutri-fact serving) consumes a Nutri-Fact and a serving and outputs the
;;  a new Nutri-Fact with values adjusted to the new serving size
;;  resize: Nutri-Fact Num -> Nutri-Fact
;;  requires: 0 < serving

;;Examples:
(check-expect (resize (make-nutri-fact "Toppables Crackers" 19 3.5 12 2 1) 38)
              (make-nutri-fact "Toppables Crackers" 38 7 24 4 2))

(define (resize nutri-fact serving)
  (make-nutri-fact
   (nutri-fact-name nutri-fact)
   serving
   (* (nutri-fact-fat nutri-fact) (/ serving (nutri-fact-serving nutri-fact)))
   (* (nutri-fact-carbs nutri-fact) (/ serving (nutri-fact-serving nutri-fact)))
   (* (nutri-fact-sugar nutri-fact)  (/ serving (nutri-fact-serving nutri-fact)))
   (* (nutri-fact-protein nutri-fact)  (/ serving (nutri-fact-serving nutri-fact)))))

;;Tests:
(check-expect (resize (make-nutri-fact "Sushi" 28 7 14 14 7) 4)
              (make-nutri-fact "Sushi" 4 1 2 2 1))


;;Part C

;;(calories nutri-fact) consumes a Nutri-Fact and outputs the amount of calories
;;  calories: Nutri-Fact -> Num

;;Examples:
(check-expect (calories (make-nutri-fact "Toppables Crackers" 19 3.5 12 2 1)) 83.5)

(define (calories nutri-fact)
  (+ (* (nutri-fact-fat nutri-fact) 9)
     (* (nutri-fact-carbs nutri-fact) 4)
     (* (nutri-fact-protein nutri-fact) 4)))

;;Tests:
(check-expect (calories (make-nutri-fact "Sushi" 28 7 14 14 7)) 147)


;;Part D

;;(choose-for-diet nutri-fact1 nutri-fact2) consumes two Nutri-Facts and selects
;;  the Nutri-Fact that is better for your friend's diet
;;  choose-for-diet: Nutri-Fact Nutri-Fact -> Nutri-Fact

;;Examples:
(check-expect (choose-for-diet (make-nutri-fact "Sushi" 28 7 14 14 7) (make-nutri-fact "Sushi" 28 7 14 14 7))
              (make-nutri-fact "Sushi" 28 7 14 14 7))

(define (choose-for-diet nutri-fact1 nutri-fact2)
  (cond
    [(< (nutri-fact-sugar (resize nutri-fact1 1))
        (nutri-fact-sugar (resize nutri-fact2 1)))
     nutri-fact1]
    [(> (nutri-fact-sugar (resize nutri-fact1 1))
        (nutri-fact-sugar (resize nutri-fact2 1)))
     nutri-fact2]
    [else
     (cond
       [(> (nutri-fact-protein (resize nutri-fact1 1))
           (nutri-fact-protein (resize nutri-fact2 1)))
        nutri-fact1]
       [(< (nutri-fact-protein (resize nutri-fact1 1))
           (nutri-fact-protein (resize nutri-fact2 1)))
        nutri-fact2]
       [else 
        (cond
          [(< (nutri-fact-carbs (resize nutri-fact1 1))
              (nutri-fact-carbs (resize nutri-fact2 1)))
           nutri-fact1]
          [(> (nutri-fact-carbs (resize nutri-fact1 1))
              (nutri-fact-carbs (resize nutri-fact2 1)))
           nutri-fact2]
          [else
           (cond
             [(> (nutri-fact-fat (resize nutri-fact1 1))
                 (nutri-fact-fat (resize nutri-fact2 1))) nutri-fact2]
             [else nutri-fact1])])])]))

;;Tests:
(check-expect (choose-for-diet (make-nutri-fact "Sushi" 28 6 14 14 7) (make-nutri-fact "Sashimi" 20 7 14 14 7))
              (make-nutri-fact "Sushi" 28 6 14 14 7))
(check-expect (choose-for-diet (make-nutri-fact "Sushi" 28 7 14 14 7) (make-nutri-fact "Trout" 28 6 14 14 7))
              (make-nutri-fact "Trout" 28 6 14 14 7))
(check-expect (choose-for-diet (make-nutri-fact "Sushi" 28 7 14 14 7) (make-nutri-fact "Rainbow Fish" 32 6 14 14 7))
              (make-nutri-fact "Rainbow Fish" 32 6 14 14 7))
(check-expect (choose-for-diet (make-nutri-fact "Sushi" 28 7 14 14 7) (make-nutri-fact "Rainbow Moose" 29 7 14 14 7))
              (make-nutri-fact "Rainbow Moose" 29 7 14 14 7))
(check-expect (choose-for-diet (make-nutri-fact "Sushi" 28 7 14 14 7) (make-nutri-fact "Chips" 28 7 14 14 2))
              (make-nutri-fact "Sushi" 28 7 14 14 7))
(check-expect (choose-for-diet (make-nutri-fact "Apple" 28 7 10 9 7) (make-nutri-fact "Doritos" 28 7 13 9 7))
              (make-nutri-fact "Apple" 28 7 10 9 7))
(check-expect (choose-for-diet (make-nutri-fact "Cookie" 28 7 14 9 7) (make-nutri-fact "Pie" 28 7 10 9 7))
              (make-nutri-fact "Pie" 28 7 10 9 7))
(check-expect (choose-for-diet (make-nutri-fact "Cake" 28 7 14 14 2) (make-nutri-fact "Tears of Students" 28 7 14 14 7))
              (make-nutri-fact "Tears of Students" 28 7 14 14 7))


;;Part E

;;(valid-nutri-fact? arbitrary-val) consumes any value called arbitrary-val and outputs
;;  true if arbitrary-val is a valid Nutri-Fact according to the data definition and false
;;  if it is not
;;valid-nutri-fact?: Any -> Bool

;;Examples:
(check-expect (valid-nutri-fact? 2) false)

(define (valid-nutri-fact? arbitrary-val)
  (and (nutri-fact? arbitrary-val)
       (string? (nutri-fact-name arbitrary-val))
       (number? (nutri-fact-serving arbitrary-val))
       (> (nutri-fact-serving arbitrary-val) 0)
       (number? (nutri-fact-fat arbitrary-val))
       (>= (nutri-fact-fat arbitrary-val) 0)
       (number? (nutri-fact-carbs arbitrary-val))
       (>= (nutri-fact-carbs arbitrary-val) 0)
       (number? (nutri-fact-sugar arbitrary-val))
       (>= (nutri-fact-sugar arbitrary-val) 0)
       (<= (nutri-fact-sugar arbitrary-val) (nutri-fact-carbs arbitrary-val))
       (number? (nutri-fact-protein arbitrary-val))
       (>= (nutri-fact-protein arbitrary-val) 0)
       (>= (nutri-fact-serving arbitrary-val)
           (+ (nutri-fact-protein arbitrary-val)
              (nutri-fact-carbs arbitrary-val)
              (nutri-fact-fat arbitrary-val)))))

;;Tests:
(check-expect (valid-nutri-fact? (make-nutri-fact "Cake" 28 7 14 14 2)) true)
(check-expect (valid-nutri-fact? (make-nutri-fact "Apple Cake" 11 7 14 14 2)) false)
(check-expect (valid-nutri-fact? (make-nutri-fact "Apple Pie" 0 7 14 14 2)) false)
(check-expect (valid-nutri-fact? (make-nutri-fact "Sparkling Jelly" 30 7 -1 14 2)) false)

