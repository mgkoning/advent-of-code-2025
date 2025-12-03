#lang racket

(require "util.rkt")

(define (read-bank line)
  (define (charnum c) (- (char->integer c) (char->integer #\0)))
  (map charnum (string->list line)))

(define (max-joltage-memo n memo bank)
  (let* ([bank-str (string-join (map number->string bank) "")]
         [key (cons n bank-str)])
    (cond [(= n 0) (cons 0 memo)]
          [(< (length bank) n) (cons 0 memo)]
          [(hash-has-key? memo key) (cons (hash-ref memo key) memo)]
          [else
            (let* ([b (car bank)]
                   [bs (cdr bank)]
                   [with-b (max-joltage-memo (- n 1) memo bs)]
                   [without-b (max-joltage-memo n (cdr with-b) bs)]
                   [b-result (* b (expt 10 (- n 1)))]
                   [result (max (+ (car with-b) b-result) (car without-b))]
                   [new-memo (hash-set (cdr without-b) key result)])
              (begin 
                (cons result new-memo)))])))

(define (solve n banks)
  (foldl + 0 (map car (map (lambda (b) (max-joltage-memo n (hash) b)) banks))))

(define (go input)
  (let ([banks (map read-bank (lines input))])
    (begin
        (printf "Part 1:~n")
        (printf "~a~n" (solve 2 banks))
        (printf "Part 2:~n")
        (printf "~a~n" (solve 12 banks))
    )))

(define sample-input "987654321111111
811111111111119
234234234234278
818181911112111")

;(go sample-input)
(go (read-input "day03.txt"))