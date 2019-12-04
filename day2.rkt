#lang racket/base
(require racket/string)

(define (read-tape filename)
    (call-with-input-file filename
        (lambda (port)
            (for/list ([x (string-split (read-line port) ",")])
               (string->number x)))))
(read-tape "day2.data")
