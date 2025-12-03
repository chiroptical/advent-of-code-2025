#lang racket

(require "util.rkt")

(define (max-by f xs)
  (for/fold ([to-return #f]
             [max-val #f]
             #:result to-return)
            ([x xs])
    (define fx (f x))
    (match max-val
      [#f (values x fx)]
      [_
       (if (> fx max-val)
           (values x fx)
           (values to-return max-val))])))

(define/match (pair->number _)
  [((cons x y)) (+ (* 10 x) y)])

(define/match (shift _pair _next)
  [((cons x y) z) (max-by pair->number (list (cons x y) (cons x z) (cons y z)))])

(define (part-1 file)
  (define lines (file->lines file))
  (for/fold ([sum 0]) ([l lines])
    (define numbers (map char->number (string->list l)))
    (define final
      (for/fold ([acc (cons 0 0)]) ([n numbers])
        (shift acc n)))
    (+ sum (pair->number final))))

(module+ test
  (require rackunit)

  (check-equal? (shift (cons 9 8) 7) (cons 9 8))
  (check-equal? (shift (cons 7 6) 8) (cons 7 8))
  (check-equal? (shift (cons 8 6) 7) (cons 8 7))
  (check-equal? (shift (cons 6 8) 7) (cons 8 7))
  (check-equal? (shift (cons 5 6) 4) (cons 6 4))
  (check-equal? (shift (cons 8 7) 8) (cons 8 8))
  (check-equal? (shift (cons 8 9) 2) (cons 9 2))

  (check-equal? (part-1 "inputs/day-3-test.txt") 357)
  (check-equal? (part-1 "inputs/day-3.txt") 17412))
