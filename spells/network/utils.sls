;;; utils.sls --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells network utils)
  (export option-ref
          let-options*
          raise-impl-restriction)
  (import (rnrs))

(define (option-ref alist key default)
  (cond ((assq key alist) => cadr)
        (else default)))

(define-syntax let-options*
  (syntax-rules ()
    ((_ opts-expr ((name default) ...) body ...)
     (let* ((opts opts-expr)
            (name (option-ref opts 'name default))
            ...)
       body ...))))

(define (raise-impl-restriction who message)
  (raise (condition
          (make-who-condition who)
          (make-implementation-restriction-violation)
          (make-message-condition message))))

)
