#lang racket

(provide part-1)

(require data/collection
         "util.rkt"
         racket/trace)

; TODO: macro which makes define -> dfn, match-define -> cases

(define (mk-range s)
  (match-define (cons begin (cons end _)) (string-split s "-"))
  (inclusive-range (string->number begin) (string->number end)))

(define (part-1? n)
  (define s (number->string n))
  (define l (string-length s))
  (define m (quotient l 2))
  (define fst-half (substring s 0 m))
  (define snd-half (substring s m))
  (equal? fst-half snd-half))

(define (eq? x y)
  (equal? (sequence->list* x) (sequence->list* y)))

(define (seq-eq? heads tails)
  (for/sequence ([h heads] [t tails]) (eq? h t)))

(trace seq-eq?)

(define (all-equal seq)
  (define heads (repeat (first seq)))
  (define tails (rest seq))
  (andmap identity (seq-eq? heads tails)))

(define (part-2? n)
  (define s (number->string n))
  (define l (string-length s))
  (define m (+ 1 (quotient l 2)))
  (for/fold ([is-invalid #f]) ([w (in-range 1 m)])
    #:break is-invalid
    (define seq (chunk w (string->list s)))
    (all-equal seq)))

(define (solve is-inv? file)
  (define lines (file->lines file))
  (define ranges (csv (first lines)))
  (define futs
    (for*/list ([range ranges]
                [id (mk-range range)])
      (future (thunk (if (is-inv? id) id 0)))))
  (for/fold ([acc 0]) ([fut futs])
    (+ acc (touch fut))))

(define (part-1 file)
  (solve part-1? file))

(define (part-2 file)
  (solve part-2? file))

(module+ test
  (require rackunit)

  (check-equal? (mk-range "11-13") '(11 12 13))
  (check-equal? (part-1? 11) #t)
  (check-equal? (part-1? 101) #f)

  (check-equal? (part-2? 2121212121) #t)
  (check-equal? (part-2? 212121212) #f)

  (check-equal? (part-1 "inputs/day-2-test.txt") 1227775554)
  (check-equal? (part-1 "inputs/day-2.txt") 19386344315)
  (check-equal? (part-2 "inputs/day-2-test.txt") 4174379265)
  (check-equal? (part-2 "inputs/day-2.txt") 34421651192))
