#lang racket/base

(require racket/list)

(define (extract-code number) (remainder number 100))

(define (extract-modes number)
    (list (remainder (quotient number 100) 10)
          (remainder (quotient number 1000) 10)
          (remainder (quotient number 10000) 10)))

(define (tape-value value tape [parameter 0])
    (if (= parameter 0) (list-ref tape value) value))

(struct Operation (tape-inc input-inc) #:transparent)
(struct Output Operation (value))
(struct WriteTape Operation (index value))

(struct Tape (tape head) #:transparent)

(define (action-output action stream)
    (if (Output? action) (append stream (list (Output-value action))) stream))
(define (action-input action stream)
    (let ([n (Operation-input-inc action)])
    (if (> n 0) (drop stream n) stream)))
(define (Tape-advance tape action)
    (Tape (Tape-tape tape) (+ (Tape-head tape) (Operation-tape-inc action))))
(define (Tape-write tape action)
    (if (WriteTape? action)
        (let* ([ta (Tape-tape tape)]
               [index (WriteTape-index action)]
               [head (take ta index)]
               [tail (drop ta (+ 1 index))]
               [value (WriteTape-value action)])
            (Tape (append head (list value) tail) (Tape-head tape)))
        tape))

; (define op-multiply (tape)
;     (let* ([modes (extract-modes (first tape))]
;            [left (tape-value )]
;         )
;     (Output 4 0 (* (tape-value  ))))

(require rackunit)

(check-equal? (extract-code 1002) 2 "extract the code")
(check-equal? (extract-modes 1002) '(0 1 0) "extract a few parameters")
(check-equal? (extract-modes 11002) '(0 1 1) "extract a few parameters")
(check-equal? (tape-value 1 '(0 5 6 7 3 2))  5 "Get by reference")
(check-equal? (tape-value 1 '(0 5 6 7 3 2) 0)  5 "Get by reference")
(check-equal? (tape-value 1 '(0 5 6 7 3 2) 1)  1 "Get by value")
(check-equal? (action-output (Operation 1 0) '(1 2)) '(1 2) "Not an outputter")
(check-equal? (action-output (Output 1 0 5) '(1 2)) '(1 2 5) "Is an outputter")
(check-equal? (action-input (Operation 1 0) '(1 2)) '(1 2) "Not a consumer")
(check-equal? (action-input (Operation 1 1) '(1 2)) '(2) "Consumes one item")
(check-equal? (action-input (Operation 1 2) '(1 2)) '() "Consumes two items")
(check-equal? (action-input (Operation 1 3) '(1 2 3 4)) '(4) "Consumes three items")
(check-equal? (Tape-advance (Tape '(1 2 3 4) 1) (Operation 1 3))
    (Tape '(1 2 3 4) 2) "Advance tape")
(check-equal? (Tape-write (Tape '(1 2 3 4) 1) (Operation 1 1))
    (Tape '(1 2 3 4) 1) "No write")
(check-equal? (Tape-write (Tape '(1 2 3 4) 1) (WriteTape 1 1 2 5))
    (Tape '(1 2 5 4) 1) "Do write")
