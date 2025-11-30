#lang racket

(require "util.rkt")

(define (solve1 input)
  42)

(define (go)
  (let ([input (read-input "day01.txt")])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (solve1 input)))))

(go)