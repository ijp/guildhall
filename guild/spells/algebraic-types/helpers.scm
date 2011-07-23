#!r6rs

(library (guild spells algebraic-types helpers)
  (export expand-datatype-dispatcher)
  (import (for (rnrs) run (meta -1))
          (guild spells string-utils)
          (guild spells syntax-utils))

(define (cond-clause k name variants variant-clause lose)
  (syntax-case variant-clause (else)
    (((variant field ...) body0 body ...)
     (let* ((variant-sym (syntax->datum #'variant))
            (variant-def (assq variant-sym variants)))
       (unless variant-def
         (lose (list "{0} is not a variant of datatype {1}" variant-sym name)
               variant-clause))
       (with-syntax
           (((field-binding ...)
             (map (lambda (field-id field-name)
                    #`(#,field-id (#,(identifier-append k #'variant "-" field-name)
                                   value)))
                  #'(field ...)
                  (cdr variant-def))))
         #`((#,(identifier-append k #'variant "?") value)
            (let (field-binding ...)
              body0 body ...)))))
    ((else body0 body ...)
     variant-clause)))

(define (expand-datatype-dispatcher name variants)
  (lambda (stx)
    (syntax-case stx ()
      ((k "cases" expr variant-clause ...)
       (let ((lose
              (lambda (message subform)
                (syntax-violation 'cases
                                  (string-substitute (car message) (cdr message))
                                  stx
                                  subform)))
             (have-else? (find (lambda (clause)
                                 (syntax-case clause (else)
                                   ((else . rest) #t)
                                   (_             #f)))
                               #'(variant-clause ...))))
         (with-syntax (((clause ...)
                        (map (lambda (clause)
                               (cond-clause #'k name variants clause lose))
                             #'(variant-clause ...)))
                       (else-clause-maybe
                        (if have-else?
                            #'()
                            #'((else
                                (assertion-violation 'cases
                                                     "no matching clause for value"
                                                     value))))))
           #'(let ((value expr))
               (cond clause ... . else-clause-maybe))))))))

)
