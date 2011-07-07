;;; format.sls --- Common-Lisp-style `format'.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Common-Lisp-style @code{format}.
(library (spells format)
  (export format)
  (import (except (rnrs base) error)
          (rnrs unicode)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs r5rs)
          (spells error)
          (spells pretty-print)
          (srfi :38 with-shared-structure)
          (spells include))

  (define char->ascii char->integer)
  (define ascii->char integer->char)
  
  (include-file ((spells private) format)))
