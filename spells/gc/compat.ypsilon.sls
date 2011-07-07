;;; gc.ypsilon.sls --- GC-interacting procedures for Ypsilon.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells gc compat)
  (export make-weak-cell weak-cell-ref weak-cell?
          make-reaper
          collect)
  (import (rnrs base)
          (rnrs control)
          (spells misc)
          (only (core)
                make-weak-mapping
                weak-mapping?
                weak-mapping-key
                weak-mapping-value
                collect))

  (define (make-weak-cell obj)
    (make-weak-mapping obj #f))

  (define (weak-cell? thing)
    (weak-mapping? thing))

  (define (weak-cell-ref weak-cell)
    (weak-mapping-key weak-cell))

  ;; Ypsilon does not (yet) support guardians or an equivalent
  ;; mechanism, see
  ;; http://code.google.com/p/ypsilon/issues/detail?id=75
  (define (make-reaper proc)
    (case-lambda
      ((obj) (unspecific))
      (()    #f)))

)
