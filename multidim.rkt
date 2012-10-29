#lang racket/base

(require (for-syntax syntax/parse 
                     racket/base 
                     racket/syntax)
         "multidim-support.rkt"
         racket/unsafe/ops)

;; An implementation of a multidimensional array library, based on sequential access
;; from TAOCP Chapter 1, Section 2.2.6.
;;
;; This provides a form called define-multidim that creates defintions for multidimensional
;; array access.


;; Compile-time helpers:
(begin-for-syntax
  (define (check-definitional-context! stx)
    (define current-context (syntax-local-context))
    (unless (or (eq? current-context 'top-level)
                (eq? current-context 'module)
                (internal-definition-context? current-context))
      (raise-syntax-error #f "multidim can only be used in a definition context" stx))))


(define-syntax (define-multidim stx)  
  
  (check-definitional-context! stx)
  
  (syntax-parse stx
    [(_ name:id dims:expr ...)
     (with-syntax ([internal-name (format-id #f "~a" #'name)]
                   [name? (format-id #'name "~a?" #'name)]
                   [name-ref (format-id #'name "~a-ref" #'name)]
                   [name-set! (format-id #'name "~a-set!" #'name)]
                   [(dim-args ...) (generate-temporaries #'(dims ...))]
                   [(ds ...) (generate-temporaries #'(dims ...))]
                   [(cs ...) (generate-temporaries #'(dims ...))])
       (with-syntax ([internal-name? (format-id #'internal-name "~a?" #'internal-name)]
                     [internal-name-data (format-id #'internal-name "~a-data" #'internal-name)])
         (syntax/loc stx
           (begin
             
             ;; We create an internal structure.  Users should not be able to construct
             ;; it directly without going through the constructor
             (struct internal-name (data) #:transparent)
             (define-values (name name? name-ref name-set!)
               (let*-values ([(ds ...) (values dims ...)]
                             [(size) (* ds ...)]
                             [(cs ...) (apply values (compute-coefficients (list ds ...)))])
                 
                 ;; Early check: make sure the integers are all exact nonnegative integers:
                 (unless (and (exact-nonnegative-integer? ds) ...)
                   (raise-type-error 'name "exact nonnegative integers" (list ds ...)))
                 
                 ;; Also make sure size doesn't go beyond indexable size:
                 (unless (fixnum? size)
                   (raise-type-error 'name "dimension product of fixnum magnitude" size))
                 
                 ;; For the public functions, we want to check our types before
                 ;; hitting the unsafe operations.
                 (define (check-entry! who a-multi dim-args ...)
                   (unless (internal-name? a-multi)
                     (raise-type-error who (symbol->string 'name) a-multi))
                   (begin (unless (and (exact-nonnegative-integer? dim-args)
                                       (< dim-args ds))
                            (raise-type-error who "indices within multidim bounds" (list dim-args ...)))
                          ...))
                 
                 ;; Constructor
                 (define (name)
                   (internal-name (make-vector size)))
                 
                 ;; Getter
                 (define (name-ref a-multi dim-args ...)
                   (check-entry! 'name-ref a-multi dim-args ...)
                   (unsafe-vector-ref (unsafe-struct-ref a-multi 0)
                                      (unsafe-fx+ (unsafe-fx* dim-args cs) ...)))
                 
                 ;; Setter
                 (define (name-set! a-multi dim-args ...)
                   (check-entry! 'name-set! a-multi dim-args ...)
                   (unsafe-vector-set! (unsafe-struct-ref a-multi 0)
                                       (unsafe-fx+ (unsafe-fx* dim-args cs) ...)
                                       v))
                 
                 (values name
                         internal-name?
                         name-ref
                         name-set!)))))))]))
