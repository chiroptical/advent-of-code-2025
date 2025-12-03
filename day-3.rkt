#lang racket

(require "util.rkt"
         racket/trace)

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
        (defn f (first final))
        (defn s (second final))
        (defn num-s (string-append (number->string f) (number->string s)))
        (defn num (string->number num-s))
        (+ sum num)))

(module+ test
  (require rackunit)

  (check-equal? (shift (cons #f #f) 1) (cons 1 #f))
  (check-equal? (shift (cons 1 #f) 2) (cons 1 2))
  (check-equal? (shift (cons 6 7) 8) (cons 7 8))
  (check-equal? (shift (cons 7 6) 8) (cons 7 8))
  (check-equal? (shift (cons 8 6) 7) (cons 8 7))
  (check-equal? (shift (cons 6 8) 7) (cons 8 7))
  (check-equal? (shift (cons 5 6) 4) (cons 5 6))
  (check-equal? (shift (cons 8 7) 8) (cons 8 8))

  ;(check-equal? (part-1 "inputs/day-3-test.txt") 357)
  )
