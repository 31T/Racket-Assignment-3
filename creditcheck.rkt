;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname creditcheck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;**************************
;;  Brendan Zhang (20720995)
;;  CS 135 Fall 2017
;;  Assignment 03, Problem 3  
;;**************************
;;


(define-struct date (year month day))
;; A Date is a (make-date Nat Nat Nat)
;; requires: year/month/day corresponds to a valid dat (in the Gregorian calendar)

(define-struct transaction (tdate amount category))
;; A Transaction is a (make-transaction Date Num Sym)

(define-struct account (name expires limit threshold exception))
;; An Account is a (make-account Str Date Num Num Sym)
;; requires: 0 < threshold < limit


;;Part A

;;(date<=? date1 date2) consumes two Dates (date1 and date2) and produces true if the
;;  first date occurs before the second Date or if they are the same Date and false
;;  if it occurs after the second Date
;; date<=?: Date Date -> Bool

;;Examples:
(check-expect (date<=? (make-date 1999 12 31) (make-date 1999 12 30)) false)

(define (date<=? date1 date2)
  (cond
    [(< (date-year date1) (date-year date2)) true]
    [(> (date-year date1) (date-year date2)) false]
    [else
     (cond
       [(< (date-month date1) (date-month date2)) true]
       [(> (date-month date1) (date-month date2)) false]
       [else
        (cond
          [(<= (date-day date1) (date-day date2)) true]
          [else false])])]))

;;Tests:
(check-expect (date<=? (make-date 1999 12 31) (make-date 1999 12 31)) true)
(check-expect (date<=? (make-date 1999 10 31) (make-date 1999 12 30)) true)
(check-expect (date<=? (make-date 2000 12 31) (make-date 1999 12 31)) false)
(check-expect (date<=? (make-date 1999 12 31) (make-date 1999 11 31)) false)


;;Part B
 
;;(approve? transaction account) consumes a Transaction and an Account and
;;  outputs true if the Transaction is approved for the Account and false if not
;;  Date is before or on the expiry Date
;;  approve?: Transaction Account -> Bool

;;Examples:
(check-expect (approve? (make-transaction (make-date 2010 09 20) 20 'food)
                        (make-account "Bob Smith" (make-date 2011 12 31) 60 50 'food))
              true) 

(define (approve? transaction account)
  (cond
    [(and (<= (transaction-amount transaction) (account-limit account))
          (date<=? (transaction-tdate transaction) (account-expires account)))
     true]
    [else false]))

;;Tests:
(check-expect (approve? (make-transaction (make-date 2010 09 20) 70 'food)
                        (make-account "Bob Smith" (make-date 2011 12 31) 60 50 'food))
                        false)
(check-expect (approve? (make-transaction (make-date 2013 09 20) 70 'food)
                        (make-account "Bob Smith" (make-date 2011 12 31) 60 50 'food))
                        false)
(check-expect (approve? (make-transaction (make-date 2010 09 20) 60 'food)
                        (make-account "Bob Smith" (make-date 2011 12 31) 60 50 'food))
                        true)


;;Part C

;;(alert? transaction account) consumes a Transaction and an Account and produces
;;  true if the Transaction creates an alert for the given Account and fale if not
;;  alert?: Transaction Account -> Bool

;;Examples:
(check-expect (alert? (make-transaction (make-date 2010 09 20) 70 'food)
                        (make-account "Bob Smith" (make-date 2011 12 31) 60 50 'food))
                        false)

(define (alert? transaction account)
  (cond
    [(and (approve? transaction account)
          (> (transaction-amount transaction) (account-threshold account))
          (not (symbol=? (transaction-category transaction) (account-exception account))))
     true]
    [else false]))

;;Tests:
(check-expect (alert? (make-transaction (make-date 2010 09 20) 50 'food)
                        (make-account "Bob Smith" (make-date 2011 12 31) 60 30 'gas))
              true)
(check-expect (alert? (make-transaction (make-date 2010 09 20) 70 'food)
                        (make-account "Bob Smith" (make-date 2021 12 31) 60 50 'food))
              false)
(check-expect (alert? (make-transaction (make-date 2010 09 20) 50 'food)
                        (make-account "Bob Smith" (make-date 2009 12 31) 60 30 'food))
              false)

