#lang racket

(require "util.rkt")

(define (divisors i)
  (define (divisor? n d)
    (zero? (remainder n d)))
  (filter (lambda (d) (divisor? i d)) (inclusive-range 1 (ceiling (/ i 2)))))

(define (invalid-part1? s)
  (define (even-len? s) (even? (string-length s)))
  (define (twice? s) 
    (let ([halfway (/ (string-length s) 2)]) (string=? (substring s 0 halfway) (substring s halfway))))
  (let ([val (number->string s)])
    (and (even-len? val) (twice? val))))

(define (invalid-part2? s)
  (define (repeated? prefix-len s)
    (let ([prefix (substring s 0 prefix-len)]
          [times (quotient (string-length s) prefix-len)])
      (and (< 1 times) (string=? s (string-join (build-list times (const prefix)) "")))))
  (let* ([val (number->string s)]
         [size (string-length val)]
         [ds (divisors size)])
    (ormap (lambda (d) (repeated? d val)) ds)))

(define (generate-invalid invalid-fn product-range)
  (let* ([from (string->number (car product-range))]
         [to (string->number(cadr product-range))]
         [all (inclusive-range from to)])
    (filter invalid-fn all)))

(define (solve invalid-fn ranges)
  (foldl + 0 (flatten (map (lambda (r) (generate-invalid invalid-fn r)) ranges))))

(define (read-ranges input)
  (define (to-range s) (string-split s "-"))
  (let* ([range-specs (string-split input ",")])
    (map to-range range-specs)))

(define (go input)
  (let ([ranges (read-ranges input)])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (solve invalid-part1? ranges))
      (printf "Part 2:~n")
      (printf "~a~n" (solve invalid-part2? ranges)))))

;; to test
;(define sample-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
;(go sample-input)

(go (read-input "day02.txt"))
