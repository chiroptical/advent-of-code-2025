#lang racket

(define (parse file)
  (define lines (file->lines file))
  (for/fold ([junctions (list)]) ([l lines])
    (match-define (list x y z) (string-split l ","))
    (cons (junction (string->number x) (string->number y) (string->number z)) junctions)))

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
        [_ (values (cons s pass) x-set y-set)])))
  (match (list found-x found-y)
    [(list #f #f) passthrough]
    [(list _ #f) (cons (set-add found-x y) passthrough)]
    [(list #f _) (cons (set-add found-y x) passthrough)]
    [(list _ _) (cons (set-union found-x found-y) passthrough)]))

; TODO:
; - gather all length 2 permutations
; - sort by their distances
; - fold
;   * acc - empty list of sets of junctions
;   * zip (in-range n) distances
;   * next
(define (part-1 file n)
  (define junctions (parse file))
  (define sorted-pairs
    (sort (combinations junctions 2)
          (lambda (a b)
            (match-define (list x y) a)
            (match-define (list m n) b)
            (< (distance x y) (distance m n)))))
  (define final-junctions
    (for/fold ([acc (list)])
              ([_ (in-range n)]
               [pair sorted-pairs])
      (match-define (list x y) pair)
      (if (empty? acc)
          (list (set x y))
          (next x y acc))))
  (println final-junctions)
  0
  ;(define sizes (sort (map set-count final-junctions)))
  ;(foldl + 0 (take sizes 3)
  )

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/day-8-test.txt" 10) 40)
  ; (check-equal? (part-1 "inputs/day-8.txt" 1000) 0)
  )
