#lang racket

(provide part-1)

(require "util.rkt"
         racket/trace)

(defn (mk-range s)
      (match-define (cons begin (cons end _)) (string-split s "-"))
      (inclusive-range (string->number begin) (string->number end)))

(defn (part-1? n)
      (defn s (number->string n))
      (defn l (string-length s))
      (defn m (quotient l 2))
      (defn fst-half (substring s 0 m))
      (defn snd-half (substring s m))
      (equal? fst-half snd-half))

(define (all-equal ls)
  (define hd (first ls))
  (for/and ([tl (rest ls)])
    (equal? hd tl)))

(define (part-2? n)
  (define s (number->string n))
  (define sl (string->list s))
  (define l (string-length s))
  (define m (+ 1 (quotient l 2)))
  (for/fold ([is-invalid #f]) ([w (in-range 1 m)])
    #:break is-invalid
    (all-equal (chunks-of sl w))))

(define (solve is-inv? file)
  (define lines (file->lines file))
  (define ranges (csv (first lines)))
  (define futs
    (for/list ([range ranges])
      (define ids (mk-range range))
      (future (thunk (filter is-inv? ids)))))
  (for/fold ([acc 0]) ([fut futs])
    (+ acc (foldl + 0 (touch fut)))))

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
