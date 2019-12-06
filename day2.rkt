#lang racket/base

(require racket/string)
(require racket/list)
(require 2htdp/abstraction)

(define (read-tape filename)
    (call-with-input-file filename
        (lambda (port)
            (for/list ([x (string-split (read-line port) ",")])
               (string->number x)))))

(define (run-op n tape)
    (when (< (* 4 n) (length tape))
        (let ([codes (take-right (take tape (* 4 (+ 1 n))) 4)])
        (unless (= 99 (first codes))
            (let ([operation (match (first codes) [1 +] [2 *])]
                  [operand-a (list-ref tape (second codes))]
                  [operand-b (list-ref tape (third codes))]
                  [location (fourth codes)])
                (append
                    (take tape location)
                    (list (operation operand-a operand-b))
                    (drop tape (min (length tape) (+ 1 location)))))))))
            
(define (run-tape tape [n 0])
    (let ([new-tape (run-op n tape)])
    (if (void? new-tape)
        (values tape)
        (run-tape new-tape (+ n 1)))))

(require rackunit)
(check-equal? (run-op 0 '(1 1 2 0)) '(3 1 2 0) "modify prior")
(check-equal? (run-op 0 '(2 4 2 4 99 6 6 6)) '(2 4 2 4 198 6 6 6) "modify post")
(check-equal? (run-op 1 '(2 4 2 4 99 6 6 6 8)) (void) "check too long")
(check-equal? (run-op 3 '(2 4 2 4 1 6 6 6 8)) (void) "check out of range")
(check-equal?
    (run-tape '(1 1 1 1 1 1 1 1 99 0 0 0 ))
    '(1 4 1 1 1 1 1 1 99 0 0 0)
    "Check running a full tape")

(define (modify-1201 tape) (append (list (first tape)) '(12 2) (drop tape 3)))

((compose first run-tape modify-1201 read-tape) "day2.data")
