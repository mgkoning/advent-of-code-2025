#lang racket

(require "util.rkt")

(define sample "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(define (read-rotation rot)
  (let ([dir (substring rot 0 1)]
        [turns (string->number (substring rot 1))])
    (if (string=? dir "L") (* -1 turns) turns)))

(define (run-rotations input)
  (define (turn-dial next acc)
    (let* ([start (car (car acc))]
           [pos (modulo (+ next start) 100)]
           [all-positions (map (lambda (a) (modulo a 100)) (cdr (inclusive-range start (+ next start) (if (negative? next) -1 1))))]
           [past-0 (count (lambda (a) (= a 0)) all-positions)])
      (cons (list pos past-0) acc)))
  (let* ([rotations (map read-rotation (lines input))]
         [stops (foldl turn-dial (list (list 50 0)) rotations)])
    stops))

(define (solve1 input)
  (let ([stops (run-rotations input)])
    (count (lambda (a) (= (car a) 0)) stops)))

(define (solve2 input)
  (let ([stops (run-rotations input)])
    (foldl + 0 (map cadr stops))))

(define (go)
  (let ([input (read-input "day01.txt")])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (solve1 input))
      (printf "Part 2:~n")
      (printf "~a~n" (solve2 input)))))

(go)