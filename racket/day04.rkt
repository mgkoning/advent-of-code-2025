#lang racket

(require "util.rkt")

(define (neighbors p)
  (append* 
    (for/list ([dx (in-range -1 2)])
      (for/list ([dy (in-range -1 2)]
                #:unless (and (= dx 0) (= dy 0)))
        (cons (+ (car p) dx) (+ (cdr p) dy))))))

(define (read-grid input)
  (define (add next acc)
    (foldl (lambda (n map) (set-add map n)) acc next))
  (foldl add (set)
    (for/list ([y (in-naturals)]
               [line (lines input)])
      (for/list ([x (in-naturals)]
                 [c line]
                 #:when (char=? c #\@))
        (cons x  y)))))

(define (find-accessible rolls)
  (define (accessible? p)
    (let* ([ns (neighbors p)]
           [adjacent (filter (lambda (m) (set-member? rolls m)) ns)])
        (< (length adjacent) 4)))
  (filter accessible? (set->list rolls)))

(define (solve1 rolls)
  (length (find-accessible rolls)))

(define (solve2 rolls)
  (define (remove-accessible rolls)
    (let ([accessible (find-accessible rolls)])
      (cond [(null? rolls) rolls]
            [(null? accessible) rolls]
            [else
             (remove-accessible
                (foldl (lambda (next acc) (set-remove acc next)) rolls accessible))])))
  (- (set-count rolls) (set-count (remove-accessible rolls))))

(define (go input)
  (let ([rolls (read-grid input)])
    (begin
        (printf "Part 1:~n")
        (printf "~a~n" (solve1 rolls))
        (printf "Part 2:~n")
        (printf "~a~n" (solve2 rolls))
    )))

(define sample-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

;(go sample-input)
(go (read-input "day04.txt"))