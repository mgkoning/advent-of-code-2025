#lang racket

(provide read-input)

(define (read-input file-name)
  (let ([file (open-input-file (string-append-immutable "../input/" file-name))])
    (port->string file)))