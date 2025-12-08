#lang racket

(provide read-input lines coord coord3 left right neighbours8 read-grid sum flip string-split-at partial2)

(struct coord (x y) #:transparent)

(struct coord3 (x y z) #:transparent)

(define/match (add-coord a b)
  [((coord xa ya) (coord xb yb)) (coord (+ xa xb) (+ ya yb))])
(define (left c)
  (add-coord c (coord -1 0)))
(define (right c)
  (add-coord c (coord 1 0)))

(define (read-input file-name)
  (let ([file (open-input-file (string-append-immutable "../input/" file-name))])
    (port->string file)))

(define (lines input)
  (string-split input "\n"))

;; Gets the 8 adject neighbours to p. Assumes p is a (coord x y)
(define (neighbours8 p)
  (append*
    (for/list ([dx (in-inclusive-range -1 1)])
      (for/list ([dy (in-inclusive-range -1 1)]
                #:unless (and (= dx 0) (= dy 0)))
        (coord (+ (coord-x p) dx) (+ (coord-y p) dy))))))

;; Reads input as a grid. Returns a list of pairs ((coord x y) . c), where c is the character at
;; position (x, y). Note that y increases downward, meaning (0, 0) is at the top left.
(define (read-grid input)
  (append*
    (for/list ([y (in-naturals)]
               [line (lines input)])
      (for/list ([x (in-naturals)]
                 [c line])
        (cons (coord x y) c)))))

(define (sum lst) (foldl + 0 lst))

(define (flip proc)
  (λ (a b) (proc b a)))

(define (string-split-at s p)
  (cons (substring s 0 p) (substring s p)))

;; partial application of a 2-arity function
(define (partial2 fn a)
  (λ (b) (fn a b)))