;;; table.sls --- Simple hash tables.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Simple hash tables.
(library (spells table)
  (export make-table
          table?
          table-ref
          table-set!
          table-walk
          table-fold
          table->alist)
  (import (rnrs base)
          (spells table compat))  

  ;;@ Return an association list that corresponds to @1.
  (define (table->alist table)
    (let ((alist '()))
      (table-walk table
                  (lambda (key value)
                    (set! alist (cons (cons key value) alist))))
      alist))

  (define (table-fold proc init table)
    (let ((result init))
      (table-walk table
                  (lambda (key value)
                    (set! result (proc key value result))))
      result))

  (define (default-failure-thunk) #f))

;; arch-tag: ebb30766-d8c9-4468-8cb5-a3ceb5c4a592
