#lang racket

(require racket/hash)
(require threading)
(require "util.rkt")

(define (read-paths input)
  (define (read-path line)
    (match-let* ([(list-rest from dest) (string-split line #px"(: | )")])
      (hash from dest)))
  (~> input
      lines
      (map read-path _)
      (foldl (flip hash-union) (hash) _)))

(struct edge (from to) #:transparent)

(define (get-edges paths)
  (append* (hash-map paths (λ (k vs) (map (λ (v) (edge k v)) vs)))))

(define (get-nodes edges)
  (remove-duplicates (append* (map (match-λ [(edge from to) (list from to)]) edges))))

(define (topo-sort paths)
  (define (no-incoming nodes edges)
    (~> nodes (filter (λ (n) (andmap (λ (e) (not (equal? n (edge-to e)))) edges)) _)))
  (define (loop edges no-inc result)
    (match no-inc 
      ['() (reverse result)]
      [(list-rest n ns)
        (let* ([to-remove (filter (λ (e) (equal? n (edge-from e))) edges)]
               [new-edges (remove* to-remove edges)]
               [new-no-inc (no-incoming (map edge-to to-remove) new-edges)])
          (loop new-edges (remove-duplicates (append new-no-inc ns)) (cons n result)))]))
  (let* ([edges (get-edges paths)]
         [no-inc (remove-duplicates (no-incoming (get-nodes edges) edges))])
    (loop edges no-inc '())))

;; topologically sort all nodes in the graph, then accumulate possible ways through the graph.
;; we keep track of possibly several counts per node, because we also want to track whether we've
;; seen the targets on a previous step.
(define (solve2 paths targets)
  (define (update n acc)
    (let* ([dests (hash-ref paths n '())]
           [prv-cnts (hash-ref acc n (hash (set) 1))]
           [cnts (cond [(set-member? targets n)
                          ;; this is one of the targets, so we mark the current counts with the node
                          (~> prv-cnts
                              hash->list
                              (map (match-λ [(cons k v) (cons (set-add k n) v)]) _)
                              make-immutable-hash)]
                       [else prv-cnts])])
      ;; update the counts in the destination nodes
      (foldl (λ (d a) (hash-update a d (λ (cur) (hash-union cur cnts #:combine +)) (hash))) acc dests)))
  (~> paths
      topo-sort
      (foldl update (hash) _)
      (hash-ref _ "out")
      (hash-ref _ targets)))

(define (solve paths start end)
  (define (walk-all to-do done)
    (match to-do
      ['() done]
      [(list-rest next remaining)
        (cond [(string=? end (car next)) (walk-all remaining (cons next done))]
              [else
                (let* ([from (car next)]
                       [reachable (hash-ref paths from '())]
                       [new-paths (map (λ (r) (cons r next)) reachable)])
                  (walk-all (append new-paths remaining) done))])]))
  (walk-all (list (list start)) '()))

(define (go input)
  (let ([paths (read-paths input)])
    (begin
      (printf "Part 1:~n")
      (printf "~a~n" (length (solve paths "you" "out")))
      (printf "Part 2:~n")
      (printf "~a~n" (solve2 paths (set "fft" "dac"))))))

(define sample-input "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(define sample-input-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

;(go sample-input)
(go (read-input "day11.txt"))

;; to create a dotfile for graphviz
(define (to-dot paths)
  (append* (hash-map paths (λ (k v) (map (λ (d) (format "~a -> ~a~n" k d)) v)))))

(define (viz input)
  (let ([paths (read-paths input)])
    (begin
      (printf "strict digraph {~n")
      (printf "you [color=blue]~n")
      (printf "svr [color=red]~n")
      (printf "dac [color=red]~n")
      (printf "fft [color=red]~n")
      (printf "out [color=red]~n")
      (for ([e (in-list (to-dot paths))])
        (printf e))
      (printf "}"))))

;(viz sample-input)
;(viz sample-input-2)
;(viz (read-input "day11.txt"))