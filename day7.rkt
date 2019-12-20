#lang racket/base

(require racket/list)
(require racket/string)
(require racket/function)
(require racket/sequence)

(struct Operation (tape-inc input) #:transparent)
(struct Output Operation (value))
(struct WriteTape Operation (index value))
(struct Exit Operation (on-exit))

(struct Tape (tape head) #:transparent)
(struct StateMachine Tape (input output operations))
(struct Booster (machines inputs outputs continuations) #:mutable #:transparent)

(define (read-data filename)
    (call-with-input-file filename
        (lambda (port)
            (map string->number (string-split (read-line port) ",")))))

(define (modify-data tape noun verb)
    (append (list (first tape)) (list noun verb) (drop tape 3)))

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
(define (action-input action stream) (Operation-input action))
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

(define (binop-writeout op tape input)
    (let ([in-0 (Tape-arg tape 1)]
          [in-1 (Tape-arg tape 2)]
          [output-index (Tape-arg tape 3 #t)])
    (WriteTape 4 input output-index (op in-0 in-1))))

(define (op-comparison op tape [input '()])
    (binop-writeout (lambda (a b) (if (op a b) 1 0)) tape input))

(define (op-multiply tape [input '()])
    (binop-writeout * tape input))
(define (op-sum tape [input '()])
    (binop-writeout + tape input))
(define (op-exit [tape '()] [input '()])
    (Exit 0 input (lambda (x) x)))
(define (op-read-write tape input)
    (let ([value (first input)]
          [input (rest input)])
    (WriteTape 2 input (Tape-arg tape 1 #t) value)))
(define (op-sync-read-write tape input input-sync)
    (when (< (length input) 1)
        (set! input (call/cc input-sync)))
    (if (void? input) (op-exit) (op-read-write tape input)))
(define (op-sync-output tape input output-sync)
    (output-sync (list (Tape-arg tape 1)))
    (Output 2 input (Tape-arg tape 1)))
(define (op-output tape [input '()])
    (Output 2 input (Tape-arg tape 1)))
(define (jump-op op tape input)
    (let ([condition (Tape-arg tape 1)]
          [location (Tape-arg tape 2)])
    (if (op condition)
        (Operation (- location (Tape-head tape)) input)
        (Operation 3 input))))

(define (make-machine tape [input '()] [ops (void)])
    (StateMachine
        (apply list tape) 0
        input (list)
        (if (void? ops)
            (make-hash (list
                (cons 1 op-sum)
                (cons 2 op-multiply)
                (cons 3 op-read-write)
                (cons 4 op-output)
                (cons 5 (curry jump-op (lambda (x) (not (= x 0)))))
                (cons 6 (curry jump-op (lambda (x) (= x 0))))
                (cons 7 (curry op-comparison <))
                (cons 8 (curry op-comparison =))
                (cons 99 op-exit)))
            ops)))

(define (modify-machine-op machine key value)
    (let ([ops (hash-copy (StateMachine-operations machine))])
        (hash-set! ops key value)
        (struct-copy StateMachine machine [operations ops])))

(define (StateMachine-action machine)
    (let* ([code (Tape-code machine)]
           [op (hash-ref (StateMachine-operations machine) code)])
     (op machine (StateMachine-input machine))))

(define (StateMachine-next machine [input (void)])
    (let* ([action (StateMachine-action machine)]
           [output (action-output action (StateMachine-output machine))]
           [input (action-input action
                        (if (void? input) (StateMachine-input machine) input))]
           [tape (Tape-write (Tape-advance machine action) action)]
           [stream (Tape-tape tape)]
           [head (Tape-head tape)]
           [result (struct-copy StateMachine machine
                        [tape #:parent Tape stream]
                        [head #:parent Tape head]
                        [input input]
                        [output output])])
     (if (Exit? action) ((Exit-on-exit action) result) (StateMachine-next result))))

(define (clone-machine machine [input '()] [output '()])
    (struct-copy StateMachine machine [input input] [output output]))

(define (run-booster machine phases [output 0])
    (if (= (length phases) 0)
        output
        (let* ([input (list (first phases) output)]
               [initial (clone-machine machine input)])
            (run-booster
                machine
                (rest phases)
                ((compose first StateMachine-output StateMachine-next) initial)))))

(define (all-settings-part-1 machine)
    (for/list ([phases (in-permutations '(0 1 2 3 4))])
        (run-booster machine phases)))

(define (all-settings-part-2 tape)
    (for/list ([phases (in-permutations '(5 6 7 8 9))])
        (let* ([booster (make-booster tape phases)])
        (Booster-next! booster)
        (first (vector-ref (Booster-inputs booster) 0)))))


(define (Booster-set-input! booster index value)
    (vector-set! (Booster-inputs booster) index value))
(define (Booster-set-continuation! booster index value)
    (vector-set! (Booster-continuations booster) index value))
(define (Booster-set-output! booster index value)
    (vector-set! (Booster-outputs booster) index value))
(define (Booster-length booster) (vector-length (Booster-machines booster)))

(define (Booster-run-continuation! booster index)
    (let ([input (vector-ref (Booster-inputs booster) index)]
          [continuation (vector-ref (Booster-continuations booster) index)])
    (Booster-set-continuation! booster index (void))
    (Booster-set-input! booster index '())
    (continuation input)))

(define (Booster-next! booster [index 0] [continuation (void)])
    (when (procedure? continuation)
        (Booster-set-continuation! booster index continuation))
    (for ([loop (in-range index (+ index (Booster-length booster)) 1)])
        (let* ([out-index (remainder loop (Booster-length booster))]
               [out (vector-ref (Booster-outputs booster) out-index)]
               [in-index (remainder (+ loop 1) (Booster-length booster))]
               [in (vector-ref (Booster-inputs booster) in-index)])
        (when (> (length out) 0)
            (Booster-set-input! booster in-index (append in out))
            (Booster-set-output! booster out-index '()))))
    (for ([cont-index (in-range (Booster-length booster))]
          [continuation (Booster-continuations booster)]
          [input (Booster-inputs booster)])
        (cond
            [(and (procedure? continuation) (> (length input) 0))
                (Booster-run-continuation! booster cont-index)]
            [(StateMachine? continuation)
                (StateMachine-next continuation input)])))

(define (make-booster tape phases)
    (let* ([machines (list->vector (map (thunk* (make-machine tape)) phases))]
           [inputs (list->vector (map (lambda (x) (list)) phases))]
           [outputs (map list phases)]
           [outputs (append (rest outputs) (list (list (first phases) 0)))]
           [booster (Booster machines inputs (list->vector outputs) (void))]
           [machines (Booster-machines booster)])
    (for ([index (in-range (Booster-length booster))])
       (vector-set! machines index
           ((compose 
               (curryr modify-machine-op 3
                    (curryr op-sync-read-write (curry Booster-next! booster index)))
               (curryr modify-machine-op 4
                    (curryr op-sync-output (curry Booster-set-output! booster index)))
               (curryr modify-machine-op 99
                    (thunk* (Exit 0 '() (thunk* (Booster-next! booster index (void)))))))
            (vector-ref (Booster-machines booster) index))))
    (set-Booster-continuations! booster (Booster-machines booster))
    booster))

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
(check-equal? (Tape-advance (Tape '(1 2 3 4) 1) (Operation 1 3))
    (Tape '(1 2 3 4) 2) "Advance tape")
(check-equal? (Tape-write (Tape '(1 2 3 4) 1) (Operation 1 1))
    (Tape '(1 2 3 4) 1) "No write")
(check-equal? (Tape-write (Tape '(1 2 3 4) 1) (WriteTape 1 '() 2 5))
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

(let* ([tape '(1 9 10 3 2 3 11 0 99 30 40 50)]
       [initial (make-machine tape '())]
       [final (StateMachine-next initial)])
 (check-equal? (Tape-tape final) '(3500 9 10 70 2 3 11 0 99 30 40 50) "day2 example"))

(let* ([tape '(1 1 1 4 99 5 6 0 99)]
       [initial (make-machine tape '())]
       [final (StateMachine-next initial)])
 (check-equal? (Tape-tape final) '(30 1 1 4 2 5 6 0 99) "day2 example"))

(let* ([tape '(2 4 4 5 99 0)]
       [initial (make-machine tape '())]
       [final (StateMachine-next initial)])
 (check-equal? (Tape-tape final) '(2 4 4 5 99 9801) "day2 example"))

(let* ([data (read-data "day2.data")]
       [1202-data (modify-data data 12 2)]
       [initial (make-machine 1202-data '())]
       [final (StateMachine-next initial)])
 (check-equal? (first (Tape-tape final)) 10566835 "day2 data"))

(let* ([data (read-data "day2.data")]
       [2347-data (modify-data data 23 47)]
       [initial (make-machine 2347-data '())]
       [final (StateMachine-next initial)])
 (check-equal? (first (Tape-tape final)) 19690720 "day2 data"))

(let* ([data '(3 9 8 9 10 9 4 9 99 -1 8)]
       [run (compose
                StateMachine-output
                StateMachine-next
                (curry make-machine data))])
 (check-equal? (run '(8)) '(1) "day5 comparison")
 (check-equal? (run '(2)) '(0) "day5 comparison"))

(let* ([data '(3 3 1107 -1 8 3 4 3 99)]
       [run (compose
                StateMachine-output
                StateMachine-next
                (curry make-machine data))])
 (check-equal? (run '(8)) '(0) "day5 comparison 2")
 (check-equal? (run '(7)) '(1) "day5 comparison 2")
 (check-equal? (run '(9)) '(0) "day5 comparison 2"))

(let* ([data '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)]
       [run (compose
                StateMachine-output
                StateMachine-next
                (curry make-machine data))])
 (check-equal? (run '(0)) '(0) "day5 jump 1")
 (check-equal? (run '(7)) '(1) "day5 jump 1")
 (check-equal? (run '(9)) '(1) "day5 jump 1"))

(let* ([data (read-data "day5.data")]
       [initial (make-machine data '(1))]
       [final (StateMachine-next initial)])
 (check-equal? (StateMachine-output final)
    '(0 0 0 0 0 0 0 0 0 5182797)
    "day5 data part 1"))

(let* ([data (read-data "day5.data")]
       [initial (make-machine data '(5))]
       [final (StateMachine-next initial)])
 (check-equal? (StateMachine-output final)
    '(12077198)
    "day5 data"))

(let* ([program '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0)]
       [machine (make-machine program)])
    (check-equal? (apply max (all-settings-part-1 machine)) 43210))

(let* ([program '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0)]
       [machine (make-machine program)])
    (check-equal? (apply max (all-settings-part-1 machine)) 54321))

(let* ([program (read-data "day7.data")]
       [machine (make-machine program)])
    (check-equal? (apply max (all-settings-part-1 machine)) 101490 "part 1"))

(let* ([program '(3 1 99)]
       [syncer (lambda (continuation) (continuation '(5)))]
       [initial (make-machine program)]
       [machine (modify-machine-op initial 3 (curryr op-sync-read-write syncer))])
    (check-equal? (Tape-tape (StateMachine-next machine)) '(3 5 99)))

(let* ([program '(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27
                  4 27 1001 28 -1 28 1005 28 6 99 0 0 5)]
       [phases '(9 8 7 6 5)]
       [booster (make-booster program phases)])
 (Booster-next! booster)
 (check-equal? (vector-ref (Booster-inputs booster) 0) '(139629729)
    "part 2 solution")
 (check-equal? (apply max (all-settings-part-2 program)) 139629729 "part 2 example"))

(let* ([program '(3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26
                  1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55
                  2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10)]
       [phases '(9 7 8 5 6)]
       [booster (make-booster program phases)])
 (Booster-next! booster)
 (check-equal? (vector-ref (Booster-inputs booster) 0) '(18216) "part 2 solution")
 (check-equal? (apply max (all-settings-part-2 program)) 18216 "part 2 example"))

(let* ([program (read-data "day7.data")] 
       [phases '(9 8 7 6 5)]
       [booster (make-booster program phases)])
 (Booster-next! booster)
 (check-equal? (vector-ref (Booster-inputs booster) 0) '(36898044) "part 2 solution")
 (check-equal? (apply max (all-settings-part-2 program)) 61019896 "part 2 example"))
