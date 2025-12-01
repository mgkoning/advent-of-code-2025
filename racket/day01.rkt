#lang racket

(require "util.rkt")

(define (read-rotation rot)
  (let ([dir (substring rot 0 1)]
        [turns (string->number (substring rot 1))])
    (if (string=? dir "L") (* -1 turns) turns)))

(define (run-rotations input)
  (define (turn-dial next acc)
    (cons (modulo (+ next (car acc)) 100) acc))
  (let* ([rotations (map read-rotation (lines input))]
         [stops (foldl turn-dial (list 50) rotations)])
    stops))

(define (solve1 input)
  (let ([stops (run-rotations input)])
    (count (lambda (a) (= a 0)) stops)))

(define (go)
  (let ([input (read-input "day01.txt")])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (solve1 input)))))

(go)