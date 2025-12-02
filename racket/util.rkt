#lang racket

(provide read-input lines)

(define (read-input file-name)
  (let ([file (open-input-file (string-append-immutable "../input/" file-name))])
    (port->string file)))

(define (lines input)
  (string-split input "\n"))
