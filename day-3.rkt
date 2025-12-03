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

(define (list->number xs)
  (define rev (reverse xs))
  (for/fold ([acc 0]
             [multiplier 1]
             #:result acc)
            ([v rev])
    (values (+ acc (* multiplier v)) (* 10 multiplier))))

(define (shift xs next size)
  (max-by list->number (combinations (append xs (list next)) size)))

(define (solve file size)
  (define lines (file->lines file))
  (for/fold ([sum 0]) ([l lines])
    (define numbers (map char->number (string->list l)))
    (define final
      (for/fold ([acc (make-list size 0)]) ([n numbers])
        (shift acc n size)))
    (+ sum (list->number final))))

(define (part-1 file)
  (solve file 2))

(define (part-2 file)
  (solve file 12))

(module+ test
  (require rackunit)

  (check-equal? (shift (list 9 8) 7 2) (list 9 8))
  (check-equal? (shift (list 7 6) 8 2) (list 7 8))
  (check-equal? (shift (list 8 6) 7 2) (list 8 7))
  (check-equal? (shift (list 6 8) 7 2) (list 8 7))
  (check-equal? (shift (list 5 6) 4 2) (list 6 4))
  (check-equal? (shift (list 8 7) 8 2) (list 8 8))
  (check-equal? (shift (list 8 9) 2 2) (list 9 2))

  (check-equal? (part-1 "inputs/day-3-test.txt") 357)
  (check-equal? (part-1 "inputs/day-3.txt") 17412)
  (check-equal? (part-2 "inputs/day-3-test.txt") 3121910778619)
  (check-equal? (part-2 "inputs/day-3.txt") 172681562473501))
