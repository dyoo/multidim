#lang racket/base

(require rackunit
         racket/block
         "multidim.rkt")



(block
 (define rows 2)
 (define cols 2)
 (define-multidim 2x2 #:dims (rows cols))
 (define m1 (2x2))
 
 (check-equal? (2x2-ref m1 0 0) 0)
 (check-equal? (2x2-ref m1 0 1) 0)
 (check-equal? (2x2-ref m1 1 0) 0)
 (check-equal? (2x2-ref m1 1 1) 0)
 
 (2x2-set! m1 0 0 "one")
 (2x2-set! m1 0 1 "two")
 (2x2-set! m1 1 0 "three")
 (2x2-set! m1 1 1 "four")
 
 (check-equal? (2x2-ref m1 0 0) "one")
 (check-equal? (2x2-ref m1 0 1) "two")
 (check-equal? (2x2-ref m1 1 0) "three")
 (check-equal? (2x2-ref m1 1 1) "four")

 ;; Going beyond the boundaries is bad.
 (check-exn exn:fail? (lambda () (2x2-ref m1 2 2)))
 
 ;; Of course, negatives should fail.
 (check-exn exn:fail? (lambda () (2x2-ref m1 -1 1)))
 
 ;; The indices need to be exact: providing inexacts should produce an exn:fail.
 (check-exn exn:fail? (lambda () (2x2-ref m1 1.0 0)))
 (check-exn exn:fail? (lambda () (2x2-ref m1 0 0.0))))



(block
 (define rows 19)
 (define cols 19)
 (define-multidim go-board #:dims (rows cols))
 (define a-board (go-board))
 (check-true (go-board? a-board))
 (go-board-set! a-board 9 9 "tengen")
 (check-equal? (go-board-ref a-board 9 9)  "tengen"))



(block
 (define-multidim knuth-example #:dims (3 5 11 3))
 (define m (knuth-example))
 (check-equal? (knuth-example-ref m 2 4 10 2) 0)
 (knuth-example-set! m 2 4 10 2
                     "last")
 (check-equal? (knuth-example-ref m 2 4 10 2) "last")
 (knuth-example-set! m 2 4 10 2 
                     0)
 (check-equal? m (knuth-example)))