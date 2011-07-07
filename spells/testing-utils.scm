;;; testing-utils.sls --- Utilities for use with trc-testing

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells testing-utils)
  (export test-one-of)
  (import (rnrs)
          (wak trc-testing))

(define-syntax test-one-of
  (syntax-rules ()
    ((test-one-of comparator-expression expected-expression actual-expression)
     (let ((comparator comparator-expression))
       (test-compare (lambda (expected-datums actual-datum)
                       (exists (lambda (e)
                                 (comparator e actual-datum))
                               expected-datums))
                     expected-expression
                     actual-expression)))))

)
