#lang racket

(require racket/hash)
(require racket/treelist)

(define (parse file)
  (define lines (file->lines file))
  (for/fold ([nums (make-immutable-hash)]
             [ops #f])
            ([l lines])
    (match-define-values (vals mode)
      (for/fold ([init (make-immutable-hash)]
                 [mode 'nums])
                ([s (string-split l)]
                 [idx (in-naturals)])
        (match s
          ["*" (values (hash-set init idx 'mul) 'ops)]
          ["+" (values (hash-set init idx 'add) 'ops)]
          [s (values (hash-set init idx (treelist (string->number s))) mode)])))
    (match mode
      ['nums
       (values
        (hash-union nums vals #:combine (lambda (l r) (treelist-append r l)))
        ops)]
      ['ops (values nums vals)])))

(define (part-1 file)
  (match-define-values (nums ops) (parse file))
  (for/fold ([acc 0]) ([kop (hash->list ops)])
    (match-define (cons k op) kop)
    (define tl (hash-ref nums k))
    (match op
      ['mul (+ acc (foldl * 1 (treelist->list tl)))]
      ['add (+ acc (foldl + 0 (treelist->list tl)))])))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-6-test.txt") 4277556)
  (check-equal? (part-1 "inputs/day-6.txt") 4951502530386))
