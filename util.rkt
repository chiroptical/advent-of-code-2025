#lang racket

(provide csv
         todo
         point
         add-points
         render-mat
         char->number
         chunks-of
         sliding
         defn
         cases
         max-by)

(define-syntax defn (make-rename-transformer #'define))

(define-syntax cases (make-rename-transformer #'define/match))

(define (csv in)
  (string-split in ","))

(define (todo)
  (error "not implemented yet"))

(struct point (r c) #:transparent)

(define/match (add-points _x _y)
  [((point x1 x2) (point y1 y2)) (point (+ x1 y1) (+ x2 y2))])

(define (render-mat mat dims)
  (match-define (point rows cols) dims)
  (for* ([r rows]
         [c cols])
    (define display-char (if (set-member? mat (point r c)) #\X #\.))
    (display display-char)
    (and (= c (- cols 1)) (display "\n"))))

(define/match (char->number _c)
  [(#\0) 0]
  [(#\1) 1]
  [(#\2) 2]
  [(#\3) 3]
  [(#\4) 4]
  [(#\5) 5]
  [(#\6) 6]
  [(#\7) 7]
  [(#\8) 8]
  [(#\9) 9])

(define (chunks-of lst k)
  (sliding lst k k))

; Borrowed from https://github.com/codereport/racket-algorithms/blob/3305cdf0d3034493801216af101519fd885dc731/main.rkt#L104-L115
(define (sliding lst size [step 1])
  (cond
    [(> step (length lst)) (error "step has to be equal to or smaller than length of the list")]
    [(= step (length lst)) (list lst)]
    [(let recur ([lst lst]
                 [len (length lst)])
       (if (>= size len)
           (if (empty? lst)
               empty
               (list lst))
           (cons (take lst size) (recur (drop lst step) (- len step)))))]))

(define (max-by f xs)
  (for/fold ([to-return #f]
             [max-val #f]
             #:result to-return)
            ([x xs])
    (define fx (f x))
    (match max-val
      [#f (values x fx)]
      [_
       (if (> fx max-val)
           (values x fx)
           (values to-return max-val))])))

(module+ test
  (require rackunit)
  (check-equal? (add-points (point 1 1) (point -1 -1)) (point 0 0) "add-points works"))
