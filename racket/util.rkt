#lang racket

(provide read-input lines neighbours8 read-grid flip)

(define (read-input file-name)
  (let ([file (open-input-file (string-append-immutable "../input/" file-name))])
    (port->string file)))

(define (lines input)
  (string-split input "\n"))

;; Gets the 8 adject neighbours to p. Assumes p is a pair '(x . y)
(define (neighbours8 p)
  (append*
    (for/list ([dx (in-inclusive-range -1 1)])
      (for/list ([dy (in-inclusive-range -1 1)]
                #:unless (and (= dx 0) (= dy 0)))
        (cons (+ (car p) dx) (+ (cdr p) dy))))))

;; Reads input as a grid. Returns a list of pairs ((x . y) . c), where c is the character at position
;; (x, y), where y increases downward (meaning (0, 0) is at the top left).
(define (read-grid input)
  (append*
    (for/list ([y (in-naturals)]
               [line (lines input)])
      (for/list ([x (in-naturals)]
                 [c line])
        (cons (cons x  y) c)))))

(define (flip proc)
  (λ (a b) (proc b a)))