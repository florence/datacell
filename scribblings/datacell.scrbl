#lang scribble/manual
@require[@for-label[datacell
                    racket/base]
         scribble/examples
         racket/sandbox]

@title{datacell}
@author{florence}
@(define datacell-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (define evil (make-evaluator 'racket))
     (evil '(require datacell))))

@defmodule[datacell]

This package gives a simple embedded dataflow language based around the notion of ``cells'', similar to the concept
of a cell in a Spreadsheet.

A simple example:

@examples[#:eval datacell-eval
          (define-cell a 0)
          (define-cell b 2)
          (define-cell c 0)
          (define-cell d (- (expt b 2) (* 4 a c)))
          d
          (set-cell! a 1)
          (set-cell! c 1)
          d]

@defform[(define-cell name body ...)]{
 Defines a new cell called @racket[name], who's value will
 be computed by @racket[body ...]. If the body uses any other cells,
 the then the value of @racket[name] will be recomputed if the value
 of those cells change.

 A cells value is computed on demand, which is to say that
 @racket[body] is only evaluated when the value of @racket[named] is needed.
 The value of a cell is almost memoized, meaning @racket[body] is only evaluated
 if the current value is not known or a cell used by the @racket[body] has changed.
}

@defform[(set-cell! cell body)]{

 Change the value in @racket[cell], which must be an identifier defined by @racket[define-cell]. The rules
 for when @racket[body] is evaluated are the same for @racket[definef-cell].
}