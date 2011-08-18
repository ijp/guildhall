;;; nested-foof-loop.scm --- 

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (sigil ext foof-loop nested)
  (export iterate*
          iterate
          iterate!
          iterate-values

          parallel
          nested

          recur*
          recur
          lazy-recur*
          lazy-recur
          recur-values

          collect-list
          collect-list-reverse
          collect-list!
          collect-list-into!
          collect-stream
          collect-vector
          collect-vector-of-length
          collect-string
          collect-string-of-length
          collect-display
          collect-sum
          collect-product
          collect-count
          collect-average
          collect-minimum
          collect-maximum)
  (import (rnrs)
          (only (rnrs mutable-pairs) set-cdr!)
          (only (srfi :1) append-reverse)
          (only (guile) include-from-path)
          (srfi :8 receive)
          (ice-9 streams)
          (sigil ext foof-loop))
  
  (define-syntax define-aux
    (syntax-rules ()
      ((_ id ...)
        (begin
          (define-syntax id
            (lambda (x)
              (syntax-violation #f "invalid use of auxiliary keyword" x 'id)))
          ...))))

  (define-aux parallel nested)

  (include-from-path "sigil/ext/inc/syn-param")
  (include-from-path "sigil/ext/inc/nested-foof-loop"))
