#lang racket

(provide part-1)

(require "util.rkt")

; TODO: macro which makes define -> dfn, match-define -> cases

(define (mk-range s)
  (match-define (cons begin (cons end _)) (string-split s "-"))
  (inclusive-range (string->number begin) (string->number end)))

(define (is-invalid? n)
  (define s (number->string n))
  (define l (string-length s))
  (define m (quotient l 2))
  (define fst-half (substring s 0 m))
  (define snd-half (substring s m))
  (equal? fst-half snd-half))

(require racket/list/grouping)

(require racket/trace)

(define/match (all-equal _)
  [((cons hd tl)) (andmap (lambda (x) (equal? hd x)) tl)])

(trace all-equal)

; TODO: Use chunk from collections
; https://docs.racket-lang.org/collections/collections-api.html#%28def._%28%28lib._data%2Fcollection..rkt%29._chunk%29%29
(define (is-invalid-2? n)
  (define s (number->string n))
  (define l (string-length s))
  (define m (quotient l 2))
  (for/fold ([is-invalid #f]) ([w (in-range 1 m)])
    #:break is-invalid
    (define ws (windows w w (string->list s)))
    (all-equal ws)))

(define (part-1 file)
  (define lines (file->lines file))
  (define ranges (csv (first lines)))
  (for/fold ([acc 0]) ([range ranges])
    (define ids (mk-range range))
    (+ acc (foldl + 0 (filter is-invalid? ids)))))

(module+ test
  (require rackunit)

  (check-equal? (mk-range "11-13") '(11 12 13))
  (check-equal? (is-invalid? 11) #t)
  (check-equal? (is-invalid? 101) #f)

  (check-equal? (is-invalid-2? 2121212121) #t)
  (check-equal? (is-invalid-2? 212121212) #f)

  (check-equal? (part-1 "inputs/day-2-test.txt") 1227775554)
  (check-equal? (part-1 "inputs/day-2.txt") 19386344315))
