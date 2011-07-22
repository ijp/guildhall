#!r6rs
;;; stexidoc.scm --- stexidoc extractors

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells private stexidoc)
  (export spells-extractors
          foreign-extractors)
  (import (rnrs)
          (ice-9 match)
          (stexidoc extract)
          (stexidoc reader))

(define (foreign:define-extractor form)
  (match (cdr (strip-non-forms form))
    ((name ('make-pointer-c-getter type))
     `((procedure (^ (name ,name) (arguments pointer offset)))))
    (else
     #f)))

(define (%->string x)
  (if (string? x) x (symbol->string x)))

(define (symbol-append . syms)
  (string->symbol (apply string-append (map %->string syms))))

(define (defrectype*-extractor form)
  (match (cdr (strip-non-forms form))
    ((name (constructor . fields) extra-fields)
     (let ((predicate (symbol-append name "?")))
       `((procedure (^ (name ,predicate) (arguments "object")))
         (procedure (^ (name ,name) (arguments ,@fields)))
         ,@(map (lambda (field)
                  `(procedure (^ (name ,(symbol-append name '- field))
                                 (arguments ,name))))
                fields))))
    (else
     #f)))

(define spells-extractors
  (extend-extractors usual-spedl-extractors
                     `((define-record-type* . ,defrectype*-extractor))))

(define foreign-extractors
  (extend-extractors spells-extractors
                     `((define . ,foreign:define-extractor))))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:
