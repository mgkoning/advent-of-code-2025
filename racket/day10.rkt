#lang racket

(require threading)
(require "util.rkt")

(struct machine (lights buttons joltage) #:transparent)

(define (read-machine line)
  (define (indexes-to-bits ixs)
    (~> ixs
        (map (λ (i) (arithmetic-shift 1 i)) _)
        (apply bitwise-ior _)))
  (define (read-button b)
    (~> b
        (substring _ 1 (- (string-length b) 1))
        (string-split _ ",")
        (map string->number _)
        indexes-to-bits))
  (match-let* ([(list ls bs joltage) (string-split line #px"(\\] | \\{)")]
               [lights (~> ls
                           (substring _ 1)
                           string->list
                           (indexes-of _ #\#)
                           indexes-to-bits)]
               [buttons (~> bs
                            string-split
                            (map read-button _))])
  (machine lights buttons (string-trim joltage "}"))))

(define (read-manual input) (~> input lines (map read-machine _)))

(define (solve1 machines)
  (define (push button state)
    (cons (bitwise-xor (car state) button) (add1 (cdr state))))
  (define (next-states buttons state)
    (map (λ (b) (push b state)) buttons))
  (define (push-buttons m states)
    (match-let* ([(machine lights buttons _) m]
                 [new-states (append* (map (λ (s) (next-states buttons s)) states))]
                 [winners (filter (λ (s) (= lights (car s))) new-states)])
      (cond [(empty? winners) (push-buttons m (remove-duplicates new-states))]
            [else (cdr (car winners))])))
  (sum (map (λ (m) (push-buttons m (list (cons 0 0)))) machines)))

(define (go input)
  (let ([machines (read-manual input)])
  (begin
    (printf "Part 1:~n")
    (printf "~a~n" (solve1 machines))
    ; (printf "Part 2:~n")
    ; (printf "~a~n" (solve2 input))
)))

(define sample-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

;(go sample-input)
(go (read-input "day10.txt"))
