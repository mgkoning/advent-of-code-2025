#lang racket

(require threading)
(require "util.rkt")

; converts columns to rows: first column becomes first row, etc.
(define/match (transpose matrix)
  [((list-rest '() _)) null]
  [(_) (cons (map car matrix) (transpose (map cdr matrix)))])

(define/match (get-op _)
  [("+") +]
  [("*") *])

(define (solve1 input)
  (define (calc p)
    (match-let* ([(list-rest op opds) (reverse p)]
                 [operands (map string->number opds)])
      (apply (get-op op) operands)))
  ; first split lines on spaces, then transpose the resulting matrix to find the problem
  (let ([problems (map string-split (lines input))])
    (sum (map calc (transpose problems)))))

(define (solve2 input)
  (define (calc problem)
    ; a bit convoluted, because the operator is sometimes directly attached to the number
    (match-let* ([(list-rest l ls) problem]
                 [(cons opd op) (string-split-at l (sub1 (string-length l)))]
                 [operands (map string->number (cons (string-trim opd) ls))])
      (apply (get-op op) operands)))
  ; transpose the entire input, then split on blank lines to construct the problems
  (let ([problems (~> input
                      lines
                      (map string->list _)
                      transpose
                      (map list->string _)
                      (map string-trim _) ; we trim here to make the split on blank lines simpler
                      (string-join _ "\n")
                      (string-split _ "\n\n")
                      (map lines _))])
    (sum (map calc problems))))

(define (go input)
  (begin
      (printf "Part 1:~n")
      (printf "~a~n" (solve1 input))
      (printf "Part 2:~n")
      (printf "~a~n" (solve2 input))
  ))

(define sample-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

;(go sample-input)
(go (read-input "day06.txt"))