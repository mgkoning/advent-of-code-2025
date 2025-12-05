#lang racket

(require "util.rkt")

(struct range (from to) #:transparent)

(define (range-length r)
  (- (+ (range-to r) 1) (range-from r)))

(define (read-range r)
  (match-let ([(list f t) (map string->number (string-split r "-"))])
    (range f t)))

(define (parse-input input)
  (let* ([parts (string-split input "\n\n")]
         [ingredients (map string->number (lines (cadr parts)))]
         [ranges (map read-range (lines (car parts)))])
    (cons ranges ingredients)))

(define (find-fresh fresh-ranges ingredients)
  (define (contains? r i)
    (and (<= (range-from r) i) (<= i (range-to r))))
  (filter (λ (i) (ormap (λ (r) (contains? r i)) fresh-ranges)) ingredients))

(define (all-fresh fresh-ranges)
  (define (trunc r acc)
    (match-let* ([(range _ boundary) (car acc)]
                 [(range from to) r])
      (cond [(<= to boundary) acc]
            [(<= from boundary) (cons (range (+ boundary 1) to) acc)]
            [else (cons r acc)])))
  (let* ([sorted-ranges (sort fresh-ranges < #:key range-from)]
         [trunced-ranges (foldl trunc (take sorted-ranges 1) (drop sorted-ranges 1))])
    (sum (map range-length trunced-ranges))))

(define (go input)
  (match-let ([(cons fresh-ranges ingredients) (parse-input input)])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (length (find-fresh fresh-ranges ingredients)))
      (printf "Part 2:~n")
      (printf "~a~n" (all-fresh fresh-ranges))
  )))

(define sample-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

;(go sample-input)
(go (read-input "day05.txt"))