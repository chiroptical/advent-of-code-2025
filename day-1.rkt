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
  (cons (new-point p amt) (quotient (+ p (abs amt)) 100)))

; (require racket/trace)

; (trace point-and-turns)

(define (part-2 file)
  (let*-values ([(lines) (read-notes file)]
                [(_point password)
                 (for/fold ([point 50]
                            [password 0])
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

  (check-equal? (point-and-turns 50 -68) (cons 82 1))
  (check-equal? (point-and-turns 98 102) (cons 0 2))
  (check-equal? (point-and-turns 95 60) (cons 55 1))
  (check-equal? (point-and-turns 50 1000) (cons 50 10))

  (check-equal? (part-1 "inputs/day-1-test.txt") 3 "part 1 test")
  (check-equal? (part-1 "inputs/day-1.txt") 984 "part 1")

  (check-equal? (part-2 "inputs/day-1-test.txt") 6 "part 2 test")
  (check-equal? (part-2 "inputs/day-1.txt") 5380 "part 2"))
