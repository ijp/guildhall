;;; os-string.sls --- Operating-system string abstraction.

;; Copyright (C) 2008-2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Handles OS-strings as used by MzScheme.

;;; Code:
#!r6rs

(library (spells pathname os-string)
  (export os-string?
          os-string->string)
  (import (rnrs base)
          (only (mzscheme) path? path->string))


  (define os-string? path?)
  (define (os-string->string os-string)
    (path->string os-string))
  
  )
