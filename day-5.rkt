#lang racket

(require data/integer-set)

(struct ingredients (fresh available) #:transparent)

(define (read-ingredients file)
  (define lines (file->lines file))
  (match-define-values (fresh available _)
    (for/fold ([fresh (make-range)]
               [available (set)]
               [mode 'ranges])
              ([l lines])
      (match mode
        ['ranges
         #:when (string=? "" l)
         (values fresh available 'value)]
        ['ranges
         (define sp (string-split l "-"))
         (match-define (list b e) sp)
         (define rng (make-range (string->number b) (string->number e)))
         (values (union rng fresh) available mode)]
        ['value
         (define x (string->number l))
         (values fresh (set-add available x) mode)])))
  (ingredients fresh available))

(define (part-1 file)
  (match-define (ingredients fresh available) (read-ingredients file))
  (for/fold ([acc 0])
            ([a available]
             #:when (member? a fresh))
    (+ 1 acc)))

(define (part-2 file)
  (match-define (ingredients fresh _available) (read-ingredients file))
  (count fresh))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-5-test.txt") 3)
  (check-equal? (part-1 "inputs/day-5.txt") 517)

  (check-equal? (part-2 "inputs/day-5-test.txt") 14)
  (check-equal? (part-2 "inputs/day-5.txt") 336173027056994))
