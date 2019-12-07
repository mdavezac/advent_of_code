#lang racket/base

(require racket/list)
(require racket/string)

(define (to_number numbers)
    (for/sum ([i (length numbers)] [n numbers])
        (* (expt 10 (- (length numbers) i 1)) n)))

(define (range-rule numbers [start 265275] [end 781584])
    (let ([x (to_number numbers)])
    (and (>= x start) (<= x end))))

(define (2-seq-rule numbers)
    (ormap = (drop-right numbers 1) (rest numbers)))

(define (2*-seq-rule numbers)
    (let  ([x (apply string
                (map (lambda (a b) (if (= a b) #\1 #\0))
                     (drop-right numbers 1) (rest numbers)))])
     (or (or
        (string-prefix? x "10")
        (string-suffix? x "01"))
        (string-contains? x "010"))))

(define (get-list)
    (for*/list
        ((a0 (range 2 8 1))
         (a1 (range a0 10 1))
         (a2 (range a1 10 1))
         (a3 (range a2 10 1))
         (a4 (range a3 10 1))
         (a5 (range a4 10 1)))
        (list a0 a1 a2 a3 a4 a5)))

(define (part-1)
    (length (filter (lambda (x) (and (range-rule x) (2-seq-rule x))) (get-list))))

(define (part-2)
    (length (filter (lambda (x) (and (range-rule x) (2*-seq-rule x))) (get-list))))

(require rackunit)
(check-equal? (to_number '(1)) 1 "1 is 1")
(check-equal? (to_number '(2)) 2 "2 is 2")
(check-equal? (to_number '(2 1)) 21 "2 1 is 21")
(check-equal? (to_number '(4 2 1)) 421 "4 2 1 is 421")
(check-equal? (range-rule '(4 2 1) 421 421) #t "range rule inside")
(check-equal? (range-rule '(4 2 1) 321 420) #f "range rule above")
(check-equal? (range-rule '(4 2 1) 422 425) #f "range rule below")
(check-equal? (2-seq-rule '(4 2 1)) #f "no two same")
(check-equal? (2-seq-rule '(6 4 4 1 5)) #t "two same")
(check-equal? (part-1) 960 "part 1")
(check-equal? (part-2) 626 "part 2")
