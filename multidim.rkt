#lang racket/base

(require (for-syntax syntax/parse 
                     racket/base 
                     racket/syntax)
         "multidim-support.rkt")

;; An implementation of a multidimensional array library
(define-syntax (multidim stx)
  
  ;; fixme: ensure we are in a definitional context.
  
  (syntax-parse stx
    [(_ name:id dims:expr ...)
     (with-syntax ([internal-name (format-id #f "~a" #'name)]
                   [name? (format-id #'name "~a?" #'name)]
                   [name-ref (format-id #'name "~a-ref" #'name)]
                   [name-set! (format-id #'name "~a-set!" #'name)]
                   [(dim-args ...) (generate-temporaries #'(dims ...))]
                   [(cs ...) (generate-temporaries #'(dims ...))])
       (with-syntax ([internal-name? (format-id #'internal-name "~a?" #'internal-name)]
                     [internal-name-data (format-id #'internal-name "~a-data" #'internal-name)])
         (syntax/loc stx
           (begin
             (struct internal-name (data) #:transparent)
             (define-values (name name? name-ref name-set!)
               (let*-values ([(ds) (list dims ...)]
                             [(size) (apply * ds)]
                             [(cs ...) (apply values (compute-coefficients ds))])
                 (values 
                  
                  (lambda ()
                    (internal-name (make-vector size)))
                  
                  (lambda (a-multi)
                    (internal-name? a-multi))
                  
                  (lambda (a-multi dim-args ...)
                    (unless (internal-name? a-multi)
                      (raise-type-error 'name-set! (symbol->string 'name) a-multi))
                    ;; FIXME: check ranges as well
                    (vector-ref (internal-name-data a-multi)
                                (+ (* dim-args cs) ...)))
                  
                  (lambda (a-multi dim-args ... v)
                    (unless (internal-name? a-multi)
                      (raise-type-error 'name-set! (symbol->string 'name) a-multi))
                    ;; FIXME: check ranges as well
                    (vector-set! (internal-name-data a-multi)
                                 (+ (* dim-args cs) ...)
                                 v)))))))))]))

(multidim matrix 3 4)
(define m (matrix))
(matrix? m)
(matrix-ref m 0 0)
(matrix-set! m 0 0 42)
(matrix-ref m 0 0)
m

(define ROWS 5)
(define COLS 25)
(multidim board ROWS COLS)
(define b (board))
(matrix? b)
(board? b)