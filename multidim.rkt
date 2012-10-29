#lang racket/base

(require (for-syntax syntax/parse 
                     racket/base 
                     racket/syntax)
         "multidim-support.rkt")

;; An implementation of a multidimensional array library
(define-syntax (multidim stx)
  (syntax-parse stx
    [(_ name:id dims:expr ...)
     (with-syntax ([(internal-name) (generate-temporaries #'(name))]
                   [name? (format-id #'name "~a?" #'name)]
                   [name-ref (format-id #'name "~a-ref" #'name)]
                   [name-set! (format-id #'name "~a-set!" #'name)]
                   [(dim-args ...) (generate-temporaries #'(dims ...))])
       (with-syntax ([internal-name? (format-id #'internal-name "~a?" #'internal-name)]
                     [internal-name-data (format-id #'internal-name "~a-data" #'internal-name)])
         (syntax/loc stx
           (begin
             (struct internal-name (data))
             (define-values (name name? name-ref name-set!)
               (let* ([ds (list dims ...)]
                      [size (apply * ds)]
                      [cs (compute-coefficients ds)])
                 (values (lambda ()
                           (internal-name (make-vector size)))
                         (lambda (a-multi)
                           (internal-name? a-multi))
                         (lambda (a-multi dim-args ...)
                           (unless (internal-name? a-multi)
                             (raise-type-error 'name-set! (symbol->string 'name) a-multi))
                           (vector-ref (internal-name-data a-multi)
                                       ;; fixme
                                        ))
                         (lambda ()
                           'setter))))))))]))

(multidim matrix 3 4)
(define m (matrix))
(matrix? m)

