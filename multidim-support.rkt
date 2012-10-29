#lang racket/base

(provide compute-coefficients)

;; Support libraries for multidimensional arrays.

(define (compute-coefficients nums)
  (cdr
   (let loop ([nums nums])
     (cond
       [(null? nums)
        (list 1)]
       [else
        (define partial-answer (loop (cdr nums)))
        (cons (* (car nums) (car partial-answer))
              partial-answer)]))))