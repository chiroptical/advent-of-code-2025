#lang racket

(define (parse file)
  (define lines (file->lines file))
  #f)

(define (part-1 file)
  (define input (parse file))
  0)

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-N-test.txt") 0)
  ; (check-equal? (part-1 "inputs/day-N.txt") 0)
  )
