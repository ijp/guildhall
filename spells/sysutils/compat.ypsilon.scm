;;; sysutils.ypsilon.sls --- Ypsilon sysutils

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells sysutils compat)
  (export find-exec-path
          host-info)
  (import (rnrs base)
          (rnrs records syntactic)
          (srfi :8 receive)
          (spells string-utils)
          (spells filesys)
          (only (core)
                architecture-feature
                getenv
                process-environment->alist)
          (only (ypsilon ffi) on-posix))

  (define (find-exec-path prog)
    (let ((paths (string-split (getenv "PATH") #\:)))
      (find-file prog paths file-executable?)))

  (define (host-info)
    (let ((os (architecture-feature 'operating-system)))
      (values
       (architecture-feature 'machine-hardware)
       "unknown"
       (cond ((string=? os "linux") "linux-gnu")
             (else                  os)))))

  )
