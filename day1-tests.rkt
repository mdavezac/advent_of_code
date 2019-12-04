#lang racket/base

(require rackunit)
(require "day2.rkt")

(check-equal? (fuel 6) 0 "check add-fuel")
(check-equal? (fuel 9) 1 "check add-fuel")
(check-equal? (fuel 2) 0 "No negative fuel")
(check-equal? (fuel -2) 0 "No negative fuel")

(check-equal? (fuel-of-fuel 1) 0 "fuel-of-fuel wishing hard")
(check-equal? (fuel-of-fuel 2) 0 "fuel-of-fuel wishing hard")
(check-equal? (fuel-of-fuel 9) 1 "fuel-of-fuel one deep")
(check-equal? (fuel-of-fuel 245) 
    (+ (fuel 245) (fuel (fuel 245)) (fuel (fuel (fuel 245))))
    "fuel-of-fuel one deep")
