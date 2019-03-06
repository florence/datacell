#lang racket/base
(provide define-cell set-cell!)
(require "private/cell.rkt"
         racket/set
         (for-syntax syntax/parse
                     racket/syntax
                     racket/base)
         racket/stxparam)
(module+ test (require rackunit))
(begin-for-syntax
  (define cells (make-parameter #f)))
(define-syntax define-cell
  (syntax-parser
    [(_ x:id body:expr ...)
     (define-values (cells bdy) (compile-cell-body #'(let () body ...)))
     #`(define-a-cell-with-parents x #,cells #,bdy)]))

(begin-for-syntax
  (struct stx-cell (id func)
    #:property
    prop:procedure (struct-field-index func)))
(define-syntax define-a-cell-with-parents
  (syntax-parser
    [(_ x:id (parents:id ...) body:expr)
     #:with y (generate-temporary #'x)
     #:with (actual-parents ...)
     (filter (lambda (x) (syntax-local-value x (lambda () #f)))
             (syntax->list #'(parents ...)))
     #`(begin
         (define y (make-cell (lambda () body) (set actual-parents ...)))
         (define-syntax x
           (stx-cell
            #'y
            (syntax-parser
              [z:id
               (when (cells)
                 (set-box! (cells) (cons (get-underlying-cell-id #'z) (unbox (cells)))))
               #'(cell-value/force! y)]))))]))

(define-for-syntax (compile-cell-body body)
  (define context (box null))
  (define compiled-cell-body (compile-cell-body-with-context body context))
  (define parents (unbox context))
  (when (cells)
    (set-box! (context) (append parents (unbox (cells)))))
  (values parents compiled-cell-body))
(define-for-syntax (compile-cell-body-with-context body bcells)
  (parameterize ([cells bcells])
    (local-expand body 'expression null)))

(define-for-syntax (get-underlying-cell-id z)
  (stx-cell-id
   (syntax-local-value
    z
    (lambda () (raise-syntax-error #f "expected something defined by define-cell" z)))))

(define-syntax set-cell!
  (syntax-parser
    [(_ c:expr body:expr ...)
     #:with actual-cell (get-underlying-cell-id #'c)
     (define-values (parents bdy)
       (compile-cell-body #'(let () body ...)))
     #`(update-cell! actual-cell (lambda () #,bdy) (set #,@parents))]))

(module+ test
  (test-case "basics"
    (define-cell a 0)
    (define-cell b 2)
    (define-cell c 0)
    (define-cell d (- (expt b 2) (* 4 a c)))
    (check-equal? d 4)
    (set-cell! a 1)
    (set-cell! c 1)
    (check-equal? d 0)))
    