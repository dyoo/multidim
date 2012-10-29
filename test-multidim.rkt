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


(block
 (define-multidim cube #:dims (3 3 3))
 (define c (cube))
 
 (for*/fold ([counter 0])
            ([i (in-range 3)]
             [j (in-range 3)]
             [k (in-range 3)])
   (cube-set! c i j k counter)
   (add1 counter))
 
 (check-equal? (for*/list ([i (in-range 3)]
                           [j (in-range 3)]
                           [k (in-range 3)])
                 (cube-ref c i j k))
               (build-list 27 (lambda (i) i))))


;; Test to see if we can set the source of the matrix and see
;; that the representation is as we expect.
(block
 (define-multidim mat #:dims (2 3))
 (define v (make-vector 6))
 (define m (mat #:source v))
 (mat-set! m 0 0 'a)
 (mat-set! m 0 1 'b)
 (mat-set! m 0 2 'c)
 (mat-set! m 1 0 'd)
 (mat-set! m 1 1 'e)
 (mat-set! m 1 2 'f)

 (check-equal? (mat-ref m 0 0) 'a)
 (check-equal? (mat-ref m 0 1) 'b)
 (check-equal? (mat-ref m 0 2) 'c)
 (check-equal? (mat-ref m 1 0) 'd)
 (check-equal? (mat-ref m 1 1) 'e)
 (check-equal? (mat-ref m 1 2) 'f)
 (check-equal? v #(a b c d e f)))




;; The source matrix must be mutable, or else we are breaking abstractions!
(block
 (define-multidim mat #:dims (2 3))
 (define an-immutable-vector #(0 0 0 0 0 0))
 (check-exn exn:fail? (lambda () (mat #:source an-immutable-vector))))
 


;; Passing an ill-formed vector in the source should also fail.
(block
 (define-multidim mat #:dims (2 100))
 (check-exn exn:fail? (lambda () (mat #:source (vector)))))