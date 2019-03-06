#lang racket/base
(provide
 make-cell
 update-cell!
 cell-value/force!)
(require racket/set
         racket/undefined)
(module+ test (require rackunit))

(struct cell ([value #:mutable]
              [thunk #:mutable]
              [recompute? #:mutable]
              children
              [parents #:mutable]
              id)
  #:methods gen:equal+hash
  [(define (equal-proc a b _)
     (eq? (cell-id a) (cell-id b)))
   (define (hash-proc a f)
     (f (cell-id a)))
   (define (hash2-proc a f)
     (f (cell-id a)))])
(struct cell-identity ()
  #:constructor-name make-cell-identity)

(define (make-cell thunk parents)
  (cell undefined thunk #t
        (weak-set) parents
        (make-cell-identity)))

(define (update-cell! cell thunk new-parents)
  (set-cell-value! cell undefined)
  (set-cell-thunk! cell thunk)
  (set-cell-parents! cell new-parents)
  (cell-mark-dirty! cell))

(define (cell-value/force! c)
  (when (cell-recompute? c)
    (set-cell-value! c ((cell-thunk c)))
    (mark-children-dirty! c)) 
  (cell-value c))

(define (cell-mark-dirty! c)
  (unless (cell-recompute? c)
    (set-cell-recompute?! c #t)
    (mark-children-dirty! c)))

(define (mark-children-dirty! c)
  (for ([ch (in-set (cell-children c))])
    (cell-mark-dirty! ch)))

(module+ test
  (check-equal?
   (cell-value/force! (make-cell (lambda () 1) (set)))
   1)
  (test-case ""
    (define a (make-cell (lambda () 1) (set)))
    (define b (make-cell (lambda () (cell-value/force! a))
                         (set a)))
    (check-equal?
     (cell-value/force! b)
     1)
    (update-cell! a (lambda () 2) (set))
    (check-equal?
     (cell-value/force! b)
     2)))
   