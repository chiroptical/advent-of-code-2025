#lang racket

(provide part-1)

(define (read-notes file)
  (file->lines file))

(define (new-point p amt)
  (modulo (+ p amt) 100))

(define (increment-password pass p)
  (if (= p 0)
      (+ 1 pass)
      pass))

(define (make-amount amt)
  (string->number (list->string amt)))

(define (part-1 file)
  (let*-values ([(lines) (read-notes file)]
                [(_point password) (for/fold ([point 50]
                                              [password 0])
                                             ([line lines])
                                     (match (string->list line)
                                       [(cons #\R amt)
                                        (let ([p (new-point point (make-amount amt))])
                                          (values p (increment-password password p)))]
                                       [(cons #\L amt)
                                        (let ([p (new-point point (- (make-amount amt)))])
                                          (values p (increment-password password p)))]))])
    password))

(define (point-and-turns p amt)
  (let* ([summation (+ p amt)]
         [next (modulo summation 100)])
    (cond
      ; moving right, simple quotient from summation
      [(positive? amt) (cons next (quotient summation 100))]

      ; moving left from zero, just use quotient from amt
      [(and (zero? p) (negative? amt)) (cons next (quotient (abs amt) 100))]

      ; hit zero or summation is negative, crossed zero add 1 to quotient of the summation
      [(or (zero? summation) (negative? summation)) (cons next (+ 1 (quotient (abs summation) 100)))]

      [else (cons next 0)])))

(define (part-2 file)
  (let* ([lines (read-notes file)]
         [password (for/fold ([point 50]
                              [password 0]
                              #:result password)
                             ([line lines])
                     (match (string->list line)
                       [(cons #\R amt)
                        (match (point-and-turns point (make-amount amt))
                          [(cons new-point turns) (values new-point (+ password turns))])]
                       [(cons #\L amt)
                        (match (point-and-turns point (- (make-amount amt)))
                          [(cons new-point turns) (values new-point (+ password turns))])]))])

    password))

(module+ test
  (require rackunit)

  (check-equal? (new-point 50 -68) 82 "returns the correct point")

  (check-equal? (point-and-turns 55 -55) (cons 0 1))
  (check-equal? (point-and-turns 1 -24) (cons 77 1))
  (check-equal? (point-and-turns 50 -68) (cons 82 1))
  (check-equal? (point-and-turns 82 -30) (cons 52 0))
  (check-equal? (point-and-turns 98 102) (cons 0 2))
  (check-equal? (point-and-turns 95 60) (cons 55 1))
  (check-equal? (point-and-turns 50 1000) (cons 50 10))
  (check-equal? (point-and-turns 50 -1000) (cons 50 10))
  (check-equal? (point-and-turns 40 -150) (cons 90 2))
  (check-equal? (point-and-turns 60 -150) (cons 10 1))

  (check-equal? (part-1 "inputs/day-1-test.txt") 3 "part 1 test")
  (check-equal? (part-1 "inputs/day-1.txt") 984 "part 1")

  (check-equal? (part-2 "inputs/day-1-test.txt") 6 "part 2 test")
  ; TODO: Something is still wrong about point-and-turns...
  (check-equal? (part-2 "inputs/day-1.txt") 5657 "part 2"))
