#lang racket/base

(require racket/string)
(require racket/function)
(require racket/list)
(require racket/set)
(require graph)

(define (parse-graph filename)
    (call-with-input-file filename
        (lambda (port)
            (directed-graph
                (for/list ([line (in-lines port)])
                    (apply list (map string->symbol (string-split line ")"))))))))


(define (centers graph)
    (let* ([edges (get-edges graph)]
           [parent (list->set (map first edges))]
           [child (list->set (map second edges))])
        (set-subtract parent child)))

(define (part-1 graph)
    (let-values ([(distances _) (bfs graph 'COM)])
        (apply + (hash-values distances))))

(require rackunit)
(check-equal? (centers (parse-graph "day6.data")) (set 'COM) "Find centers")
(check-equal? (part-1 (parse-graph "day6.data")) 122782 "Part 1")

