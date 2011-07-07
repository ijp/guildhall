;;; compat.ikarus.sls --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells include compat)
  (export stale-when
          annotation?
          annotation-expression
          read-annotated)
  (import (rnrs base)
          (only (ikarus)
                stale-when
                annotation?
                read-annotated
                annotation-expression))

)
