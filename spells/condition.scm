;;; condition.scm --- Extra condition types

;; Copyright (C) 2008, 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Additional condition types.
(library (spells condition)
  (export &parser-error make-parser-error parser-error?
          parser-error-port
          &stacked make-stacked-condition stacked-condition? next-condition

          dsp-condition
          display-condition
          
          ;; This doesn't really belong here
          limited-write)
  (import (rnrs)
          (wak foof-loop)
          (wak fmt)
          (only (guile) include-from-path))
  
  (include-from-path "spells/private/condition"))
