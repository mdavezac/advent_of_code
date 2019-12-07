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
                (for/list ([x (string-split line ",")])
                    (parse x))))))

(define (subpath-increment subpath)
    (Location 
        (match (Subpath-direction subpath) [#\R 1] [#\L -1] [else 0])
        (match (Subpath-direction subpath) [#\U 1] [#\D -1] [else 0])))

(define (subpath-move subpath [origin (Location 0 0)])
    (let ([delta (subpath-increment subpath)])
    (Location
        (+ (* (Location-x delta) (Subpath-distance subpath)) (Location-x origin))
        (+ (* (Location-y delta) (Subpath-distance subpath)) (Location-y origin)))))

(define (subpath-points subpath [origin (Location 0 0)])
    (let ([delta (subpath-increment subpath)])
    (for/set ([i (in-range 1 (+ 1 (Subpath-distance subpath)) 1)])
        (Location
            (+ (* i (Location-x delta)) (Location-x origin))
            (+ (* i (Location-y delta)) (Location-y origin))))))

(define (path-points path [origin (Location 0 0)])
    (define-values (_ pos)
        (for/fold ([gamma origin]
                   [positions (set origin)])
                  ([subpath path])
            (values (subpath-move subpath gamma)
                    (set-union positions (subpath-points subpath gamma)))))
    pos)

(define (Location-manhattan location)
    (+ (abs (Location-x location)) (abs (Location-y location))))

(require rackunit)
(check-equal? (Location 1 2) (Location 1 2) "compare structs")
(check-equal? (Location-y (Location 1 2)) 2 "access struct fields")
(check-equal? (parse "R80") (Subpath #\R 80) "parse stuff")
(check-equal?
    (subpath-points (Subpath #\R 2))
    (set (Location 1 0) (Location 2 0))
    "Can run a sub-path")
(check-equal?
    (subpath-points (Subpath #\D 3))
    (set (Location 0 -1) (Location 0 -2) (Location 0 -3))
    "Can run a sub-path downwards")
(check-equal?
    (path-points (list (Subpath #\L 2)))
    (set (Location 0 0) (Location -1 0) (Location -2 0))
    "Single subpath")
(check-equal?
    (path-points (list (Subpath #\L 2) (Subpath #\U 1)))
    (set (Location 0 0) (Location -1 0) (Location -2 0) (Location -2 1))
    "Two subpaths")
(check-equal?
    (path-points (list (Subpath #\R 2) (Subpath #\U 1) (Subpath #\L 1)))
    (set (Location 0 0) (Location 1 0) (Location 2 0) (Location 2 1) (Location 1 1))
    "Three subpaths")
(check-equal? (Location-manhattan (Location 1 -3)) 4 "Manhattan distance")

((compose
    (curry apply min)
    (curry map Location-manhattan)
    set->list
    (curryr set-remove (Location 0 0))
    (curry apply set-intersect)
    (curry map path-points)
    parse-paths) "day3.data")
