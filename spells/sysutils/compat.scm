;;; compat.scm --- Guile sysutils compatibility

;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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
          (spells filesys)
          (only (guile)
                uname
                utsname:machine
                utsname:sysname
                getenv
                string-split))

  (define (find-exec-path prog)
    (let ((paths (string-split (getenv "PATH") #\:)))
      (find-file prog paths file-executable?)))

  (define (host-info)
    (let ((uts (uname)))
      (values (utsname:machine uts)
              "unknown"
              (utsname:sysname uts))))

  )
