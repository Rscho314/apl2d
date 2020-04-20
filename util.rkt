#lang racket

(provide euclidean-distance)

(define (euclidean-distance p1 p2)
  (let ([x1 (first p1)]
        [y1 (second p1)]
        [x2 (first p2)]
        [y2 (second p2)])
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))