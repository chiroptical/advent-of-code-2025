#lang racket

(define/match (range-check _r _x)
  [((range start end) x) (and (>= x start) (<= x end))])

(struct range (start end) #:transparent)

(define (or-ranges ranges x)
  (ormap (lambda (r) (range-check r x)) ranges))

(struct ingredients (fresh available) #:transparent)

(define (read-ingredients file)
  (define lines (file->lines file))
  (match-define-values (fresh available _)
    (for/fold ([fresh (list)]
               [available (set)]
               [mode 'ranges])
              ([l lines])
      (match mode
        ['ranges
         (if (string=? "" l)
             (values fresh available 'value)
             (let ([sp (string-split l "-")])
               (match sp
                 [(list b e)
                  (values (cons (range (string->number b) (string->number e)) fresh)
                          available
                          mode)])))]
        ['value (let ([x (string->number l)]) (values fresh (set-add available x) mode))])))
  (ingredients fresh available))

(define (part-1 file)
  (match-define (ingredients fresh available) (read-ingredients file))
  (for/fold ([acc 0]) ([a available])
    (if (or-ranges fresh a)
        (+ 1 acc)
        acc)))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-5-test.txt") 3)
  (check-equal? (part-1 "inputs/day-5.txt") 517))
