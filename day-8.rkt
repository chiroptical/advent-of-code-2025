#lang racket

(define (parse file)
  (define lines (file->lines file))
  (for/fold ([junctions (list)]) ([l lines])
    (match-define (list x y z) (string-split l ","))
    (cons (junction (string->number x) (string->number y) (string->number z))
          junctions)))

(struct junction (x y z) #:transparent)

(define (distance a b)
  (match-define (junction x1 y1 z1) a)
  (match-define (junction x2 y2 z2) b)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)) (sqr (- z2 z1)))))

(define (next x y los)
  (match-define-values (passthrough found-x found-y)
    (for/fold ([pass (list)]
               [x-set #f]
               [y-set #f])
              ([s los])
      (define x-set-mem (set-member? s x))
      (define y-set-mem (set-member? s y))
      (match (list x-set-mem y-set-mem)
        [(list #t #t) (values (cons s pass) x-set y-set)]
        [(list #t _) (values pass s y-set)]
        [(list _ #t) (values pass x-set s)]
        [(list #f #f) (values (cons (set x y) (cons s pass)) x-set y-set)])))
  (match (list found-x found-y)
    [(list #f #f) passthrough]
    [(list _ #f) (cons (set-add found-x y) passthrough)]
    [(list #f _) (cons (set-add found-y x) passthrough)]
    [(list _ _) (cons (set-union found-x found-y) passthrough)]))

(require graph)

(define (compare a b)
  (match-define (list x y) a)
  (match-define (list m n) b)
  (< (distance x y) (distance m n)))

(require data/heap)

(define (part-1 file n)
  (define junctions (parse file))

  ; heap is binary tree where insertions of nodes
  ; maintain the comparison function you provide.
  ; In this case, we want to order by minimum distance
  (define sorted-pairs (make-heap compare))
  (for ([junction (in-combinations junctions 2)])
    (heap-add! sorted-pairs junction))

  (define g (unweighted-graph/undirected '()))
  (for ([_idx (in-range n)]
        [pair (in-heap sorted-pairs)])
    (match-define (list x y) pair)
    (add-edge! g x y))
  (define sizes (sort (map set-count (cc g)) >))
  (foldl * 1 (take sizes 3)))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-8-test.txt" 10) 40)
  (check-equal? (part-1 "inputs/day-8.txt" 1000) 24360))
