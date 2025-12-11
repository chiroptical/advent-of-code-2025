#lang racket

(require "util.rkt"
         data/heap
         racket/trace)

(define (parse file)
  (define lines (file->lines file))
  (for/fold ([acc (list)]) ([l lines])
    (match-define (list x y) (csv l))
    (cons (point (string->number x) (string->number y)) acc)))

(define (area a b)
  (match-define (point x1 y1) a)
  (match-define (point x2 y2) b)
  (define x-side (+ 1 (abs (- x2 x1))))
  (define y-side (+ 1 (abs (- y2 y1))))
  (match (list x-side y-side)
    [(list x y)
     #:when (= x 0)
     y]
    [(list x y)
     #:when (= y 0)
     x]
    [(list x y) (* x y)]))

(define (part-1 file)
  (define points (parse file))
  (define largest-area (make-heap >))
  (for ([combo (in-combinations points 2)])
    (match-define (list x y) combo)
    (heap-add! largest-area (area x y)))
  (for/first ([val (in-heap largest-area)])
    val))

(module+ test
  (require rackunit)

  (check-equal? (area (point 2 5) (point 11 1)) 50)

  (check-equal? (part-1 "inputs/day-9-test.txt") 50)
  (check-equal? (part-1 "inputs/day-9.txt") 4746238001))
