#lang racket

(require "util.rkt")

(define (parse file)
  (define lines (file->lines file))
  (for/fold ([start (point 0 0)]
             [splitters (list)]
             [_max-row 0])
            ([ln lines]
             [row (in-naturals)])
    (for/fold ([s start]
               [sp splitters]
               [_max-row 0])
              ([c (string->list ln)]
               [col (in-naturals)])
      (match c
        [#\S (values (point row col) sp row)]
        [#\^ (values s (cons (point row col) sp) row)]
        [_ (values s sp row)]))))

(define (splitters-in-row splitters row)
  (filter (match-lambda
            [(point r _c) (= r row)])
          splitters))

(define (split-beams beams splits)
  (for/fold ([new-beams (set)]
             [count 0])
            ([beam (set->list beams)])
    (define match?
      (ormap (lambda (s) (equal? (add-points beam (point 1 0)) s)) splits))
    (if match?
        (values (set-union (set (add-points beam (point 1 1))
                                (add-points beam (point 1 -1)))
                           new-beams)
                (+ count 1))
        (values (set-add new-beams (add-points beam (point 1 0))) count))))

(define (part-1 file)
  (match-define-values (start splitters max-row) (parse file))
  (for/fold ([beams (set start)]
             [counts 0]
             #:result counts)
            ([row (in-range 1 max-row)])
    (define beam-splits (splitters-in-row splitters row))
    (match-define-values (new-beams count) (split-beams beams beam-splits))
    (values new-beams (+ count counts))))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-7-test.txt") 21)
  (check-equal? (part-1 "inputs/day-7.txt") 1642))
