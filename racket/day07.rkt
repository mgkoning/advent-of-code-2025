#lang racket

(require threading)
(require "util.rkt")

(define (read-manifold input)
  (define (find-char-coords c grid)
    (~> grid
        (filter (λ (p) (char=? c (cdr p))) _)
        (map car _)))
  (match-let* ([grid (read-grid input)]
               [(list start) (find-char-coords #\S grid)]
               [splitters (find-char-coords #\^ grid)])
    (cons start splitters)))

(define (find-next-splitter manifold from)
  ;; assumes manifold is primarily sorted by increasing y
  (match-let* ([(coord x y) from]
               [splitters (filter (match-λ [(coord sx sy) (and (= sx x) (< y sy))]) manifold)])
    (cond [(null? splitters) null]
          [else (take splitters 1)])))

(define (cast-beam start manifold)
  (define (find-all-splitters to-visit visited result sources)
    (cond
      [(set-empty? to-visit) (cons result sources)]
      [else
        (let* ([from (set-first to-visit)]
               [froms (set-rest to-visit)]
               [new-visited (set-add visited from)]
               [next-splitter (find-next-splitter manifold from)])
          (cond
            [(null? next-splitter) (find-all-splitters froms new-visited result sources)]
            [else
              (let* ([splitter (car next-splitter)]
                     [splits (list (left splitter) (right splitter))]
                     [insert-or-append-source (λ (n acc) (hash-update acc n (λ (s) (set-add s from)) (set from)))]
                     [new-sources (foldl insert-or-append-source sources splits)]
                     [unvisited (filter (λ (s) (not (set-member? visited s))) splits)]
                     [new-froms (foldl (flip set-add) froms unvisited)])
                (find-all-splitters new-froms new-visited (set-add result splitter) new-sources))]))]))
  (find-all-splitters (set start) (set) (set) (hash)))

(define (count-paths start source-map)
  (define/match (y-then-x a b)
    [((coord xa ya) (coord xb yb)) (if (= ya yb) (< xa xb) (< ya yb))])
  ;; Determine the number of paths that can be taken to the exits. Exits are nodes that are
  ;; not a predecessor of any node. The number of paths is the sum of the paths that lead to the
  ;; predecessors of the exits, so first we count the paths to all the nodes.
  (let* ([all-preds (foldl set-union (set) (hash-values source-map))]
         [nodes (sort (hash-keys source-map) y-then-x)]
         [exits (filter (λ (f) (not (set-member? all-preds f))) nodes)]
         [get-counts (λ (n counts)
                        (let ([count (~> n
                                         (hash-ref source-map _)
                                         (set-map _ (partial2 hash-ref counts))
                                         (sum _))])
                          (hash-set counts n count)))]
         [path-counts (foldl get-counts (hash start 1) nodes)])
    (~> exits
        (map (partial2 hash-ref path-counts) _)
        sum)))

(define (go input)
  (match-let* ([(cons start manifold) (read-manifold input)]
               [(cons splitters source-map) (cast-beam start manifold)])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (set-count splitters))
      (printf "Part 2:~n")
      (printf "~a~n" (count-paths start source-map))
  )))

(define sample-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

;(go sample-input)
(go (read-input "day07.txt"))