#lang racket/base

(require racket/string)
(require racket/list)
(require racket/set)
(require 2htdp/abstraction)
(require racket/function)

(struct Subpath (direction distance) #:transparent)
(struct Location (x y) #:transparent)

(define (parse text)
    (let ([direction (first (string->list text))]
          [distance (string->number (list->string (rest (string->list text))))])
    (Subpath direction distance)))

(define (parse-paths filename)
    (call-with-input-file filename
        (lambda (port)
            (for/list ([line (in-lines port)])
                (map parse (string-split line ","))))))

(define (subpath-increment subpath)
    (Location 
        (match (Subpath-direction subpath) [#\R 1] [#\L -1] [else 0])
        (match (Subpath-direction subpath) [#\U 1] [#\D -1] [else 0])))

(define (Location-+ a b)
    (Location (+ (Location-x a) (Location-x b))
              (+ (Location-y a) (Location-y b))))

(define (subpath-move subpath [origin (Location 0 0)])
    (let ([delta (subpath-increment subpath)])
    (Location
        (+ (* (Location-x delta) (Subpath-distance subpath)) (Location-x origin))
        (+ (* (Location-y delta) (Subpath-distance subpath)) (Location-y origin)))))

(define (subpath-points subpath [origin (Location 0 0)])
    (let ([delta (subpath-increment subpath)])
    (rest
        (for/fold ([path (list origin)])
                  ([i (in-range (Subpath-distance subpath))])
            (append path (list (Location-+ delta (last path))))))))

(define (path-points path [origin (Location 0 0)])
    (foldl
        (lambda (left right) (append right (subpath-points left (last right))))
        (list origin)
        path))

(define (Location-manhattan location)
    (+ (abs (Location-x location)) (abs (Location-y location))))

(require rackunit)
(check-equal? (Location 1 2) (Location 1 2) "compare structs")
(check-equal? (Location-+ (Location -1 1) (Location 1 -1)) (Location 0 0) "+")
(check-equal? (Location-y (Location 1 2)) 2 "access struct fields")
(check-equal? (parse "R80") (Subpath #\R 80) "parse stuff")
(check-equal?
    (subpath-points (Subpath #\R 2))
    (list (Location 1 0) (Location 2 0))
    "Can run a sub-path")
(check-equal?
    (subpath-points (Subpath #\D 3))
    (list (Location 0 -1) (Location 0 -2) (Location 0 -3))
    "Can run a sub-path downwards")
(check-equal?
    (path-points (list (Subpath #\L 2)))
    (list (Location 0 0) (Location -1 0) (Location -2 0))
    "Single subpath")
(check-equal?
    (path-points (list (Subpath #\L 2) (Subpath #\U 1)))
    (list (Location 0 0) (Location -1 0) (Location -2 0) (Location -2 1))
    "Two subpaths")
(check-equal?
    (path-points (list (Subpath #\R 2) (Subpath #\U 1) (Subpath #\L 1)))
    (list (Location 0 0) (Location 1 0) (Location 2 0) (Location 2 1) (Location 1 1))
    "Three subpaths")
(check-equal? (Location-manhattan (Location 1 -3)) 4 "Manhattan distance")

((compose
    (curry apply min)
    (curry map Location-manhattan)
    set->list
    (curryr set-remove (Location 0 0))
    (curry apply set-intersect)
    (curry map (compose list->set path-points))
    parse-paths) "day3.data")
