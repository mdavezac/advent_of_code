#lang racket/base

(define (extract-code number) (remainder number 100))

(define (extract-parameters number)
    (list (remainder (quotient number 100) 10)
          (remainder (quotient number 1000) 10)
          (remainder (quotient number 10000) 10)))

(define (tape-value value tape [parameter 0])
    (if (= parameter 0) (list-ref tape value) value))

(define-struct Outputter (value increment))
(define-struct Taper (location value increment))
(define-struct NoOper (increment))

(define (actionner action tape output) )

(require rackunit)

(check-equal? (extract-code 1002) 2 "extract the code")
(check-equal? (extract-parameters 1002) '(0 1 0) "extract a few parameters")
(check-equal? (extract-parameters 11002) '(0 1 1) "extract a few parameters")
