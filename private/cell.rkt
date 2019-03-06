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
    (define identity (cell-id c))
    (when (continuation-mark-set-first
           (current-continuation-marks)
           identity)
      (error 'datacell "re-entrant cell! A cycle in cell dependencies was crossed while evaluating some cell."))
    (set-cell-value! c
                     (with-continuation-mark identity #t
                       ((cell-thunk c))))
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
  (test-case "in which we chain cells"
    (define a (make-cell (lambda () 1) (set)))
    (define b (make-cell (lambda () (cell-value/force! a))
                         (set a)))
    (check-equal?
     (cell-value/force! b)
     1)
    (update-cell! a (lambda () 2) (set))
    (check-equal?
     (cell-value/force! b)
     2))
  (test-case "in which we have a cycle in the dataflow graph"
    (define a (make-cell (lambda () 1) (set)))
    (update-cell! a (lambda () (cell-value/force! a)) (set a))
    (check-exn
     #rx"re-entrant cell!"
     (lambda () (cell-value/force! a)))))
   