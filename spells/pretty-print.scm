;;; pretty-print.sls --- Pretty-print S-expressions.

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ A simple pretty-printer for S-expressions.
(library (spells pretty-print)
  (export pretty-print)
  (import (rnrs base)
          (srfi :38 with-shared-structure))

  ;;@defun pretty-print object [ port ]
  ;; Ouput a pretty-printed representation of @var{object} to @var{port},
  ;; which defaults to @code{(current-output-port)}.
  ;;@end defun
  (define (pretty-print object . port-opt)
    (apply write-with-shared-structure object port-opt)))

