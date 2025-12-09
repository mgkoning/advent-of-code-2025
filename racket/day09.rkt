#lang racket

(require threading)
(require "util.rkt")

(define (read-coord s)
  (match (~> (string-split s ",") (map string->number _))
    [(list x y) (coord x y)]))

(define (calc-area p)
  (define (side a b) (~> (- a b) abs add1))
  (match p [(cons (coord xa ya) (coord xb yb)) (* (side xa xb) (side ya yb))]))

(define (solve input)
  (let* ([tiles (~> input lines (map read-coord _))]
         [areas (~> tiles pairs (map calc-area _))])
    (~> areas (sort _ >) car)))

(define (between a b c)
  (if (< a b) (and (< a c) (< c b)) (and (< b c) (< c a))))

(define/match (contains rect p)
  [((cons (coord xa ya) (coord xb yb)) (coord xp yp)) (and (between xa xb xp) (between ya yb yp))])

(define (solve2 input)
  (let* ([tiles (~> input lines (map read-coord _))]
        ;; needs to account for intersecting lines
        ;  [rects (pairs tiles)]
        ;  [poss (filter (lambda (r) (andmap (lambda (p) (not (contains r p))) (remove* (list (car r) (cdr r)) tiles))) rects)]
        ;  [the-max (apply max (map calc-area poss))]
        ;; these two coordinates jumped out when visualizing my input, see (canvas)
         [cand1 (coord 94607 50076)]
         [cand2 (coord 94607 48713)]
         [partners1 (reverse (takef tiles (lambda (t) (not (equal? t cand1)))))]
         [rects1 (map (lambda (p) (cons p cand1)) partners1)]
         [poss1 (filter (lambda (r) (andmap (lambda (p) (and (not (equal? p (cdr r))) (not (contains r p)))) partners1)) rects1)]
         [max1 (apply max (map calc-area poss1))]
         [partners2 (drop (dropf tiles (lambda (t) (not (equal? t cand2)))) 1)]
         [rects2 (map (lambda (p) (cons p cand2)) partners2)]
         [poss2 (filter (lambda (r) (andmap (lambda (p) (and (not (equal? p (cdr r))) (not (contains r p)))) partners2)) rects2)]
         [max2 (apply max (map calc-area poss2))])
    (max max1 max2)))

;; for visualizing using html canvas
(define (canvas input)
  (~> input lines (map read-coord _) (map (match-lambda [(coord x y) (format "  ctx.lineTo(~a, ~a);" (/ x 10.0) (/ y 10.0))]) _) (string-join _ "\n")))

(define (go input)
  (begin
    (printf "Part 1:~n")
    (printf "~a~n" (solve input))
    (printf "Part 2:~n")
    (printf "~a~n" (solve2 input))
    ;(printf (canvas input))
))

(define sample-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

;(go sample-input)
(go (read-input "day09.txt"))
