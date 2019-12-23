#lang racket/base

(require racket/string)
(require racket/list)
(require math/array)
(require racket/function)
(require racket/sequence)

(define (read-data filename)
    (call-with-input-file filename
        (lambda (port)
            (map string->number (rest (drop-right (string-split (read-line port) "") 1))))))

(define (list->image data [width 25] [height 6])
    (let* ([flat (list->array data)]
           [size (array-size flat)]
           [layers (quotient size (* width height))]
           [result (array-reshape flat (vector layers height width))])
    result))

(define (min-x-layer image [x 0])
    (let* ([shape (array-shape image)]
           [new-shape
                (vector (vector-ref shape 0)
                        (* (vector-ref shape 1) (vector-ref shape 2)))]
           [2d (array-reshape image new-shape)]
           [counted (array->list (array-axis-count 2d 1 (curry = x)))]
           [zippy (map cons counted (sequence->list (range (length counted))))])
    (cdr (argmin car zippy))))

(define (part-1 image)
    (let* ([layer-index (min-x-layer image 0)]
           [layer (array-axis-ref image 0 layer-index)]
           [flat (array-reshape layer (vector (array-size layer)))])
    (* (array-ref (array-axis-count flat 0 (curry = 1)) #[])
       (array-ref (array-axis-count flat 0 (curry = 2)) #[]))))

(define (reducer size proc)
    (foldr
        (lambda (a b) (if (= a 2) b a))
        2
        (map proc (sequence->list (range size)))))

(define (part-2 image)
    (let ([folded (array-axis-reduce image 0 reducer)])
    (for ([i (in-range (vector-ref (array-shape folded) 0))])
        (let* ([folded (array->list (array-axis-ref folded 0 i))]
               [joined (string-join (map number->string folded) "")]
               [replace-0 (string-replace joined "0" " ")]
               [replace-1 (string-replace replace-0 "1" "*")])
        (displayln replace-1)))))



(require rackunit)

(let* ([data '(0 0 3 4 0 0 7 0 9 1 0 3)]
       [image (list->image data 3 2)])
    (check-equal? (array-shape image) #[2 2 3])
    (check-equal? (min-x-layer image) 1)
    (check-equal? (min-x-layer image 4) 1)
    (check-equal? (min-x-layer image 9) 0))

(let* ([data (read-data "day8.data")]
       [image (list->image data 25 6)])
    (check-equal? (part-1 image) 1620)
    (println (part-2 image)))
