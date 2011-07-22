#!r6rs
;;; finite-types.scm --- Scheme48-style finite types

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Based on code from Scheme48 1.8, Copyright (c) 1993-2008 by Richard
;; Kelsey and Jonathan Rees.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Types with a finite number of instances.
(library (guildhall spells finite-types)
  (export define-enumerated-type
          define-finite-type
          finite-type-case)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) expand)
          (srfi :9 records))

;;@subheading Introduction

;; @i{(This section was derived from work copyrighted @copyright{}
;; 1993--2005 by Richard Kelsey, Jonathan Rees, and Mike Sperber.)}

;; The library @code{(guildhall spells finite-types)} has two macros for
;; defining @dfn{finite} or @dfn{enumerated record types}.  These are
;; record types for which there is a fixed set of instances, all of
;; which are created at the same time as the record type itself..
;;
;;@deffn syntax define-enumerated-type
;; @lisp
;; (define-enumerated-type @var{dispatcher} @var{type}
;;   @var{predicate}
;;   @var{instance-vector}
;;   @var{name-accessor}
;;   @var{index-accessor}
;;   (@var{instance-name}
;;    @dots{}))@end lisp
;; This defines a new record type, to which @var{type} is bound, with as
;; many instances as there are @var{instance-name}s.  @var{Predicate} is
;; defined to be the record type's predicate.  @var{Instance-vector} is
;; defined to be a vector containing the instances of the type in the same
;; order as the @var{instance-name} list.  @var{Dispatcher} is defined to
;; be a macro of the form (@var{dispatcher} @var{instance-name}); it
;; evaluates to the instance with the given name, which is resolved at
;; macro-expansion time.  @var{Name-accessor} & @var{index-accessor} are
;; defined to be unary procedures that return the symbolic name & index
;; into the instance vector, respectively, of the new record type's
;; instances.
;;
;; For example,
;;
;; @lisp
;; (define-enumerated-type colour :colour
;;   colour?
;;   colours
;;   colour-name
;;   colour-index
;;   (black white purple maroon))
;;
;; (colour-name (vector-ref colours 0))    @result{} black
;; (colour-name (colour white))            @result{} white
;; (colour-index (colour purple))          @result{} 2@end lisp
;;@end deffn
  (define-syntax define-enumerated-type
    (syntax-rules ()
      ((_ dispatcher rtd
          predicate
          instance-vector
          name-accessor
          index-accessor
          (instance-name ...))
       (define-finite-type dispatcher rtd () predicate instance-vector
         name-accessor index-accessor ((instance-name) ...)))))
  
;;@deffn syntax define-finite-type
;; @lisp
;; (define-finite-type @var{dispatcher} @var{type}
;;   (@var{field-tag} @dots{})
;;   @var{predicate}
;;   @var{instance-vector}
;;   @var{name-accessor}
;;   @var{index-accessor}
;;   (@var{field-tag} @var{accessor} [@var{modifier}])
;;   @dots{}
;;   ((@var{instance-name} @var{field-value} @dots{})
;;    @dots{}))@end lisp
;; This is like @code{define-enumerated-type}, but the instances can also
;; have added fields beyond the name and the accessor.  The first list of
;; field tags lists the fields that each instance is constructed with, and
;; each instance is constructed by applying the unnamed constructor to the
;; initial field values listed.  Fields not listed in the first field tag
;; list must be assigned later.

;; For example,

;; @lisp
;; (define-finite-type colour :colour
;;   (red green blue)
;;   colour?
;;   colours
;;   colour-name
;;   colour-index
;;   (red   colour-red)
;;   (green colour-green)
;;   (blue  colour-blue)
;;   ((black    0   0   0)
;;    (white  255 255 255)
;;    (purple 160  32 240)
;;    (maroon 176  48  96)))
;;
;; (colour-name (colour black))            @result{} black
;; (colour-name (vector-ref colours 1))    @result{} white
;; (colour-index (colour purple))          @result{} 2
;; (colour-red (colour maroon))            @result{} 176@end lisp
;;@end deffn
  (define-syntax define-finite-type
    (lambda (stx)
      (syntax-case stx ()
        ((_ dispatcher rtd
            instance-fields
            predicate
            instance-vector
            name-accessor
            index-accessor
            (field-tag . field-get&set) ...
            ((instance-name . instance-field-values) ...))
         ;; the outer `with-syntax' is there just to work around a
         ;; Guile psyntax issue, see <http://savannah.gnu.org/bugs/?31472>.
         (with-syntax (((constructor) (generate-temporaries '(constructor))))
           (with-syntax (((constructor-invocation ...)
                          (let loop ((invocations '())
                                     (i 0)
                                     (names #'(instance-name ...))
                                     (values #'(instance-field-values ...)))
                            (if (null? names)
                                (reverse invocations)
                                (loop (cons #`(constructor '#,(car names)
                                                           #,i
                                                           #,@(car values))
                                            invocations)
                                      (+ i 1)
                                      (cdr names)
                                      (cdr values))))))
             #'(begin
                 (define-record-type rtd
                   (constructor name index . instance-fields)
                   predicate
                   (name name-accessor)
                   (index index-accessor)
                   (field-tag . field-get&set) ...)
                 (define instance-vector
                   (vector constructor-invocation ...))
                 (define-dispatch dispatcher (instance-name ...) instance-vector))))))))

  (define-syntax define-dispatch
    (lambda (stx)
      (syntax-case stx ()
        ((_ name (instance-name ...) instance-vector)
         #`(define-syntax name
             (lambda (stx)
               (syntax-case stx ()
                 ((dispatcher-name tag)
                  (let loop ((names (syntax->datum #'(instance-name ...)))
                             (i 0))
                    (cond ((null? names)
                           (syntax-violation (syntax->datum #'dispatcher-name)
                                             "no such instance"
                                             (syntax->datum #'tag)))
                          ((eq? (syntax->datum #'tag) (car names))
                           #`(vector-ref instance-vector #,i))
                          (else
                           (loop (cdr names) (+ i 1)))))))))))))


;;@deffn syntax finite-type-case
;;
;; @lisp
;; (finite-type-case @var{dispatcher} @var{expr} @var{clause} @dots{})@end lisp
;;
;; Similarly to R5RS @code{case}, dispatches according to an instance
;; of a finite type.  After evaluating @var{expr}, the resulting value
;; is checked against each @var{clause}, which have the same syntax as
;; a @code{case} clause, but instead of literals must contain only
;; instance names for the finite type specified by @var{dispatcher}.
;;
;; For example, 
;;
;; @lisp
;; (define (classify c)
;;   (finite-type-case colour c
;;     ((black white) 'not-a-real-colour)
;;     ((purple maroon) 'real-colour)
;;     (else 'unknown)))
;;
;; (classify (colour black))  @result{} not-a-real-colour
;; (classify (colour maroon)) @result{} real-color
;; (classify 4)               @result{} unknown@end lisp
;;
;;@end deffn
  (define-syntax finite-type-case
    (syntax-rules (else)
      ((_ dispatcher value-expr ((name ...) expr ...) ... (else alt-expr ...))
       (let ((val value-expr))
         (cond ((or (eq? (dispatcher name) val) ...)
                expr ...) ...
               (else
                alt-expr ...))))
      ((_ dispatcher value-expr ((name ...) expr ...) ...)
       (let ((val value-expr))
         (cond ((or (eq? (dispatcher name) val) ...)
                expr ...) ...)))))

)
