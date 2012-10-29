#lang racket/base

(require (for-syntax syntax/parse 
                     racket/base 
                     racket/syntax)
         "multidim-support.rkt"
         racket/unsafe/ops)


(provide define-multidim)



;; An implementation of a multidimensional array library, based on sequential access
;; from The Art of Computer Programming, Chapter 1, Section 2.2.6.
;;
;; This provides a form called define-multidim that creates multidimensional
;; array access definitions.
;;
;;
;; Usage: (define-multidim id #:dims (dim ...))
;;
;;      where each dim should be an expression that evaluates to a positive integer.
;;
;; This defines a constructor "id", a predicate "id?", a getter "id-ref", and a setter "id-set!".


;; Compile-time helpers:
(begin-for-syntax
  (define (check-definitional-context! stx)
    (define current-context (syntax-local-context))
    (unless (or (eq? current-context 'top-level)
                (eq? current-context 'module)
                (and (list? current-context) (or (liberal-define-context? (car current-context))
                                                 (internal-definition-context? current-context))))
      (raise-syntax-error #f "multidim can only be used in a definition context" stx))))



;; A little macro to rewrite the application of a binary operator on a fixed set of arguments.
(define-syntax (reduce stx)
  (syntax-parse stx
    [(_ op:expr args:expr ...)
     (let loop ([args (syntax->list #'(args ...))])
       (cond
         [(null? (cdr args))
          (car args)]
         [else
          (with-syntax ([head (car args)]
                        [reduced-rest (loop (cdr args))])
            #'(op head reduced-rest))]))]))


(define-syntax (define-multidim stx)  
  
  (check-definitional-context! stx)
  
  (syntax-parse stx
    [(_ name:id #:dims (dims:expr ...))
     (with-syntax ([internal-name (format-id #f "~a" (syntax-e #'name))]
                   [name? (format-id #'name "~a?" (syntax-e #'name) #:source #'name)]
                   [name-ref (format-id #'name "~a-ref" (syntax-e #'name) #:source #'name)]
                   [name-set! (format-id #'name "~a-set!" (syntax-e #'name) #:source #'name)]
                   [(index-args ...) (generate-temporaries #'(dims ...))]
                   [(ds ...) (generate-temporaries #'(dims ...))]      ;; dimensions
                   [(cs ...) (generate-temporaries #'(dims ...))])     ;; coefficients
       (with-syntax ([predicate (format-id #'internal-name "~a?" #'internal-name)])
         (syntax/loc stx
           (begin
             
             ;; We create an internal structure.  Users should not be able to construct
             ;; it directly without going through the constructor.  We take advantage
             ;; of this encapsulation later on in the use of the "unsafe" operations.             
             (struct internal-name (data) #:transparent)
             
             ;; We'll export a constructor, predicate, getter, and setter...
             ;; (and unsafe variations of the accessors)
             (define-values (name name? name-ref name-set!)
               
               ;; First, do some computations up front.
               (let*-values ([(ds ...) (values dims ...)]
                             [(size) (* ds ...)]
                             [(cs ...) (apply values (compute-coefficients (list ds ...)))])
                 
                 ;; Early check: make sure the dimensions are all exact positive integers:
                 (unless (and (exact-positive-integer? ds) ...)
                   (raise-type-error 'name "exact positive integers" (list ds ...)))
                 
                 ;; Also make sure size doesn't go beyond indexable size.  We're using
                 ;; a sequential representation, but there are limits to the size of a
                 ;; vector.
                 (unless (fixnum? size)
                   (raise-type-error 'name "product of all dimensions representable as a fixnum" size))                 
                 
                 ;; We'll want a small helper to validate entry into the public functions.
                 ;; We want to check our types before hitting the unsafe operations.
                 (define-syntax-rule (check-entry! who a-multi index-args ...)
                   (begin
                     (unless (predicate a-multi)
                       (raise-type-error who (symbol->string 'name) a-multi))
                     (unless (and (exact-nonnegative-integer? index-args)
                                  (< index-args ds))
                       (raise-type-error who "indices within multidim bounds" (list index-args ...)))
                     ...))
                 
                 ;; Constructor
                 (define (name #:source [v (make-vector size)])
                   (unless (and (vector? v)
                                (not (immutable? v))
                                (>= (vector-length v) size))
                     (raise-type-error 'name (format "mutable vector of length >= ~a" size) v))
                   (internal-name v))
                 
                 ;; Getter
                 (define (name-ref a-multi index-args ...)
                   (check-entry! 'name-ref a-multi index-args ...)
                   (unsafe-vector-ref (unsafe-struct-ref a-multi 0)
                                      (reduce unsafe-fx+ (unsafe-fx* index-args cs) ...)))
                 
                 ;; Setter
                 (define (name-set! a-multi index-args ... v)
                   (check-entry! 'name-set! a-multi index-args ...)
                   (unsafe-vector-set! (unsafe-struct-ref a-multi 0)
                                       (reduce unsafe-fx+ (unsafe-fx* index-args cs) ...)
                                       v))
                 
                 (values name
                         predicate
                         name-ref
                         name-set!)))))))]))