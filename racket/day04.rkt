#lang racket

(require "util.rkt")

(define (read-map input)
  (list->set
    (map car
      (filter (λ (v) (char=? #\@ (cdr v))) (read-grid input)))))

(define (find-accessible rolls)
  (define (accessible? p)
    (let* ([ns (neighbours8 p)]
           [adjacent (filter (λ (m) (set-member? rolls m)) ns)])
        (< (length adjacent) 4)))
  (filter accessible? (set->list rolls)))

(define (solve1 rolls)
  (length (find-accessible rolls)))

(define (solve2 rolls)
  (define (remove-accessible rolls)
    (let ([accessible (find-accessible rolls)])
      (cond [(null? rolls) rolls]
            [(null? accessible) rolls]
            [else (remove-accessible
                    (foldl (flip set-remove) rolls accessible))])))
  (- (set-count rolls) (set-count (remove-accessible rolls))))

(define (go input)
  (let ([rolls (read-map input)])
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