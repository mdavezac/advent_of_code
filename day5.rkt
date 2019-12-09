#lang racket/base

(require racket/list)

(struct Operation (tape-inc input-inc) #:transparent)
(struct Output Operation (value))
(struct WriteTape Operation (index value))
(struct Exit Operation ())

(struct Tape (tape head) #:transparent)

(define (Tape-code tape)
    (remainder (list-ref (Tape-tape tape) (Tape-head tape)) 100))

(define (Tape-modes tape)
    (let ([number (list-ref (Tape-tape tape) (Tape-head tape))])
    (list (remainder (quotient number 100) 10)
          (remainder (quotient number 1000) 10)
          (remainder (quotient number 10000) 10))))

(define (Tape-arg tape arg [output #f])
    (let ([head (Tape-head tape)]
          [stream (Tape-tape tape)])
    (if (= arg 0)
        (list-ref stream head)
        (let ([mode (list-ref (Tape-modes tape) (- arg 1))]
              [value (list-ref stream (+ head arg))])
            (if (and (= mode 0) (not output))
                (list-ref stream value)
                value)))))

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

(define (binop-writeout op tape)
    (let ([in-0 (Tape-arg tape 1)]
          [in-1 (Tape-arg tape 2)]
          [output-index (Tape-arg tape 3 #t)])
    (WriteTape 4 0 output-index (op in-0 in-1))))

(define (op-multiply tape) (binop-writeout * tape))
(define (op-sum tape) (binop-writeout + tape))

(require rackunit)

(check-equal? (Tape-code (Tape '(1002 1 1) 0)) 2 "extract the code")
(check-equal? (Tape-code (Tape '(1 1012 1 1) 1)) 12 "extract the code")
(check-equal? (Tape-modes (Tape '(1002 1 1) 0)) '(0 1 0) "extract a few parameters")
(check-equal? (Tape-modes (Tape '(1 11002 1 1) 1)) '(0 1 1) "extract a few parameters")
(check-equal? (Tape-arg (Tape '(100 5 6 7 3 2) 0) 1) 5 "Get by value")
(check-equal? (Tape-arg (Tape '(1 1005 6 7 3 2) 1) 2) 7 "Get by value")
(check-equal? (Tape-arg (Tape '(0 5 6 4 3 2) 0) 1) 2 "Get by reference")
(check-equal? (Tape-arg (Tape '(1 105 6 2 3 2) 1) 2) 6 "Get by reference")
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
(let ([tape (Tape '(1 2 3 4 5 6 7) 0)])
    (check-equal?
        (Tape-tape (Tape-write tape (op-multiply tape)))
        '(1 2 3 4 12 6 7)
        "Param mode multiply"))
(let ([tape (Tape '(1 2 3 2 2 6 7) 1)])
    (check-equal?
        (Tape-tape (Tape-write tape (op-multiply tape)))
        '(1 2 6 2 2 6 7)
        "Param mode multiply"))
(let ([tape (Tape '(1 102 3 2 2 6 7) 1)])
    (check-equal?
        (Tape-tape (Tape-write tape (op-multiply tape)))
        '(1 102 9 2 2 6 7)
        "Param mode multiply"))
