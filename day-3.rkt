#lang racket

(require "util.rkt"
         racket/trace)

(cases (pair->number _) [((cons x y)) (+ (* 10 x) y)])

; TODO: misinterpreted this function, we need the max of all combinations
; i.e. (x y) (x z) (y z)
; Additionally, we shouldn't start from (#f #f), just use (0 0)
(cases (shift _pair _next)
       [((cons #f #f) z) (cons z #f)]
       [((cons x #f) z) (cons x z)]
       [((cons x y) z) #:when (or (> z x) (> z y)) (cons (max x y) z)]
       [((cons x y) _) (cons x y)])

(defn (part-1 file)
      (defn lines (file->lines file))
      (for/fold ([sum 0]) ([l lines])
        (defn numbers (map char->number (string->list l)))
        (defn final
              (for/fold ([acc (cons #f #f)]) ([n numbers])
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

  (check-equal? (part-1 "inputs/day-3-test.txt") 357))
