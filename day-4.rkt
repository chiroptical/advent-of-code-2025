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

(define (remove rolls)
  (for/fold ([acc (set)]) ([r rolls])
    (define count
      (for/fold ([c 0]) ([k kernel])
        (if (set-member? rolls (add-points r k))
            (+ 1 c)
            c)))
    (if (< count 4)
        (set-add acc r)
        acc)))

(define (part-1 file)
  (define lines (file->lines file))
  (define rolls (parse-lines lines))
  (define removed (remove rolls))
  (set-count removed))

(define (part-2 file)
  (define lines (file->lines file))
  (define rolls (parse-lines lines))
  (define final
    (for/fold ([acc rolls]
               [terminate #f]
               #:result acc)
              ([_ (in-naturals)])
      #:break terminate
      (define removed (remove acc))
      (if (= (set-count removed) 0)
          (values acc #t)
          (values (set-subtract acc removed) #f))))
  (- (set-count rolls) (set-count final)))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-4-test.txt") 13)
  (check-equal? (part-1 "inputs/day-4.txt") 1363)

  (check-equal? (part-2 "inputs/day-4-test.txt") 43)
  (check-equal? (part-2 "inputs/day-4.txt") 8184))
