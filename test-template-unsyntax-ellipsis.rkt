#lang racket
(require "template-unsyntax-ellipsis.rkt"
         syntax/stx
         syntax/parse/experimental/template)

#;(syntax-case #'((1 2) (3 4)) ()
    [((x ...) ...)
     (escape (stx-map (λ (xᵢ) (+ (syntax-e xᵢ) 1))
                      (template (x ...))))])

(syntax-case #'((1 2) (3 4)) ()
  [((x ...) ...)
   (escape (stx-map (λ (xᵢ)
                      (define-syntax (a stx)
                        (datum->syntax stx (string->symbol xᵢ)))
                      (a)
                      (+ (syntax-e xᵢ) 1))
                    #'(x ...)))])
