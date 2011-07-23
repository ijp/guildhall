#!r6rs
;;; algebraic-types.scm --- EOPL-style algebraic datatypes

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Algebraic data types.  The syntax provided in this library allows
;; for concise creation of a number of record types that are
;; considered to be ``variants'' of a common (abstract) type.  Besides
;; facilitating creation of these related record types, the library
;; also offers a convenient way to dispatch and destructure instances
;; thereof.
(library (guild spells algebraic-types)
  (export define-datatype cases)
  (import (rnrs)
          (for (guild spells algebraic-types helpers) expand))

;;@defspec define-datatype name variant-clause ...
;;
;; Defines an algebraic data type identified by @var{name}.  Each
;; @var{variant-clause}, which has the syntax @code{(@var{variant}
;; (@var{field} ...))}, defines a record type as with R6RS'
;; @code{define-record-type} using its implicit accessor naming
;; scheme.  Consider this example:
;; 
;; @lisp
;; (define-datatype <expression>
;;   (if-expression (test consequent alternative))
;;   (application (operator operands))) 
;; @end lisp
;;
;; This will lead to the creation of two record types, named
;; @code{if-expression} and @code{application}, with associated
;; constructors and accessors such as @code{make-if-expression} and
;; @code{application-operands}.
;;
;;@end defspec
(define-syntax define-datatype
  (lambda (stx)
    (syntax-case stx ()
      ((k name (variant (field ...)) ...)
       #'(begin
           (define-record-type variant
             (fields field ...))
           ...
           (define-syntax name
             (expand-datatype-dispatcher
              (syntax->datum #'name)
              (syntax->datum #'((variant field ...) ...)))))))))

;;@defspec cases datatype expression clause ...
;;
;; Dispatch on instances of the record types created by
;; @ref{define-datatype}; first @var{expression} is evaluated, and the
;; result is checked against each @var{clause}.  A @var{clause} may
;; specify a a variant, with the syntax @code{((@var{variant}
;; @var{field} ...) body ...)}.  When the value of @var{expression} is
;; an instance of @var{variant}, @var{body} is evaluated in the scope
;; of the identifiers given by as @var{field}; each @var{field}
;; identifier is bound to the corresponding field of the record
;; instance.
;;
;; Additionally, an clause ``else'' with syntax @code{(else @var{body}
;; ...)}  may be given as last clause.  If none of the variants
;; specified in the other clauses produces a match, the @var{body} of
;; the else clause evaluated.  If there is no match and no ``else''
;; clause, an assertion violation is raised.
;; 
;;@end defspec
(define-syntax cases
  (syntax-rules ()
    ((_ datatype expr clause ...)
     (datatype "cases" expr clause ...))))

)

