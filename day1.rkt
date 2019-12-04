#lang racket/base

(provide fuel total-fuel fuel-of-fuel)

(define (fuel mass) (max 0 (- (quotient mass 3) 2)))
(define (fuel-of-fuel current-fuel)
    (let ([new-fuel (fuel current-fuel)])
    (if (= new-fuel 0)
        0
        (+ new-fuel (fuel-of-fuel new-fuel)))))

(define (total-fuel port)
    (apply + (for/list ([line (in-lines port)])
        (let ([x (fuel (string->number line))])
        (+ x (fuel-of-fuel x))))))

(call-with-input-file "day1.data" total-fuel)
