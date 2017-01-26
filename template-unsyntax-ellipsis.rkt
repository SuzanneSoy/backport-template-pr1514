#lang racket

(require (for-syntax phc-toolkit/untyped
                     racket/contract
                     racket/private/sc)
         syntax/parse/experimental/template
         (prefix-in backport: backport-template-pr1514/experimental/template))
(provide escape)

(begin-for-syntax
  (require racket/syntax)
  (define/with-syntax ooo #'(... ...)))

(define-syntax (mysyntax stx)
  (syntax-case stx ()
    [(_ v)
     #'(syntax (ooo v))]))

(define-for-syntax (force-expand e)
  (define e1 (local-expand e 'expression (list #'quote
                                               #'syntax
                                               #'template
                                               #'backport:template)))
  ;(displayln (list (syntax->datum e) (syntax->datum e1)))
  (syntax-case e1 (syntax template backport:template
                          begin quote set! #%plain-lambda
                          ;; TODO:
                          case-lambda let-values
                          letrec-values if begin0 with-continuation-mark
                          letrec-syntaxes+values #%plain-app #%expression #%top
                          #%variable-reference)
    [(begin _ ...)
     (datum->syntax e1 (map force-expand (syntax->list e1)) e1 e1)]
    [(set! _ ...)
     (datum->syntax e1 (map force-expand (syntax->list e1)) e1 e1)]
    [(#%plain-lambda args body ...)
     (datum->syntax e1 (list (stx-car e1)
                             #'args
                             (stx-map force-expand #'(body ...)))
                    e1 e1)]
    [(quote _)
     e1]
    [(syntax . rest)
     (displayln (syntax->datum #'rest))
     #`(quote-syntax #,e1)]
    [(template . rest)
     (displayln (syntax->datum #'rest))
     #`(quote-syntax #,e1)]
    [(backport:template . rest)
     (displayln (syntax->datum #'rest))
     #`(quote-syntax #,e1)]
    [(_ ...)
     (datum->syntax e1 (map force-expand (syntax->list e1)) e1 e1)]
    [_
     e1]))



;make-syntax-mapping syntax-pattern-variable?
;syntax-mapping-depth syntax-mapping-valvar

(define-syntax (escape stx)
  (syntax-case stx ()
    [(_ body)
     (force-expand #'body)]))

;; Doesn't work.
#;(begin
    (define-for-syntax exn-ellipses/c
      (struct/c exn:fail:syntax
                (regexp-match/c
                 #px"syntax: too few ellipses for pattern variable in template")
                any/c
                (list/c identifier?)))
    (define-syntax (escape stx)
      (syntax-case stx ()
        [(_ body)
         (let ()
           (define used-pvars '())
           (define (push-pvar! pvar)
             (set! used-pvars (cons pvar used-pvars)))
           ;(let loop ([used-pvars '()])
           (with-handlers ([exn-ellipses/c
                            (λ (exn)
                              ;; Can't do syntax-local-value :-(
                              (displayln (syntax-local-value (car (exn:fail:syntax-exprs exn))))
                              (push-pvar! (car (exn:fail:syntax-exprs exn))))])
             (local-expand #'body 'expression (list)))
           ;(displayln (syntax-pattern-variable? (syntax-local-value (car used-pvars))))
           (with-handlers ([exn-ellipses/c
                            (λ (exn)
                              (push-pvar! (car (exn:fail:syntax-exprs exn))))])
             (displayln (local-expand #'body 'expression (list))))
           #'(void))])))
