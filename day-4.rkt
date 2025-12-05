#lang racket

(require "util.rkt")

(define (parse-lines lines)
  (for/fold ([o (set)])
            ([l lines]
             [r (in-naturals)])
    (for/fold ([i o])
              ([char (string->list l)]
               [c (in-naturals)])
      (match char
        [#\. i]
        [#\@ (set-add i (point r c))]))))

(define kernel
  (let ([s (for/fold ([acc (set)]) ([p (cartesian-product '(1 -1 0) '(1 -1 0))])
             (match-define (list r c) p)
             (set-add acc (point r c)))])
    (set-remove s (point 0 0))))

(define (part-1 file)
  (define lines (file->lines file))
  (define rolls (parse-lines lines))
  (for/fold ([acc 0]) ([r rolls])
    (define count
      (for/fold ([c 0]) ([k kernel])
        (if (set-member? rolls (add-points r k))
            (+ 1 c)
            c)))
    (if (< count 4)
        (+ 1 acc)
        acc)))

(define (part-2 file)
  (define lines (file->lines file))
  (define rolls (parse-lines lines))
  (define removals
    (for/fold ([init rolls]
               [final (set)]
               [terminate #f]
               #:result final)
              ([x (in-naturals)])
      ; TODO: remove (> x 100)
      #:break (or (> x 100) terminate)
      ; TODO:
      ; - for part-1, abstraction which returns set of points to remove
      ; - remove those points from init
      ; - if init and final are the same terminate
      (values init final #f)))
  (set-count removals))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-4-test.txt") 13)
  (check-equal? (part-1 "inputs/day-4.txt") 13)

  (check-equal? (part-2 "inputs/day-4-test.txt") 43))
