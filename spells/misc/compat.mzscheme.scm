;;; compat.mzscheme.sls --- Misc functions, mzscheme compatibility

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells misc compat)
  (export sleep-seconds exit scheme-implementation)
  (import (rnrs base)
          (only (mzscheme)
                sleep exit))

  (define sleep-seconds sleep)
  (define (scheme-implementation) 'mzscheme))
