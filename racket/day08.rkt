#lang racket

(require threading)
(require "util.rkt")

(define (read-junctions input)
  (define (to-junction s)
    (~> s
        (string-split _ ",")
        (map string->number _)
        (match [(list x y z) (coord3 x y z)])))
  (~> input
      lines
      (map to-junction _)))

(struct connection (a b dist) #:transparent)

(define/match (make-pair a b)
  [((coord3 xa ya za) (coord3 xb yb zb)) (connection a b (sqrt (+ (sqr (- xa xb)) (sqr (- ya yb)) (sqr (- za zb)))))])

(define/match (make-pairs lst)
  [((list)) (list)]
  [((list _)) (list)]
  [((list a b)) (list (make-pair a b))]
  [((list-rest a as)) (append (map (λ (b) (make-pair a b)) as) (make-pairs as))])

(define (connect conn circuits)
  (match conn
    [(connection a b _)
      (cond [(null? circuits) (list (set a b))]
            [else
              (let-values ([(connected other) (partition (λ (c) (or (set-member? c a) (set-member? c b))) circuits)])
                (match connected
                  ['() (cons (set a b) other)]
                  [_ (cons (apply set-union (cons (set a b) connected)) other)]))])]))


(define (solve1 junctions)
  (let* ([circuits (foldl connect '() junctions)])
    (~> circuits
        (map set-count _)
        (sort _ >)
        (take _ 3)
        (foldl * 1 _))))

(define (solve2 junctions by-distance)
  (define (do circuits connections)
    (match connections
      [(list-rest c cs) 
        (let ([new-circuits (connect c circuits)])
          (match new-circuits
            [(list v) #:when (= (set-count v) (length junctions)) c]
            [_ (do new-circuits cs)]))]))
  (let ([last-connection (do '() by-distance)])
    (match last-connection [(connection (coord3 xa _ _) (coord3 xb _ _) _) (* xa xb)])))

(define (go input bound)
  (match-let* ([junctions (read-junctions input)]
               [pairs (make-pairs junctions)]
               [by-distance (sort pairs < #:key connection-dist)])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (solve1 (take by-distance bound)))
      (printf "Part 2:~n")
      (printf "~a~n" (solve2 junctions by-distance))
  )))

(define sample-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

;(go sample-input 10)
(go (read-input "day08.txt") 1000)