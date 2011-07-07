;;; cells.sls --- Implementation of the cells datatype in terms of records

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Heavyweight (but portable) implementation of cells.

;;; Code:
#!r6rs

;;@ Mutable cells.
(library (spells cells)
  (export make-cell cell? cell-ref cell-set!)
  (import (rnrs base)
          (srfi :9 records))

  ;;@defun make-cell value
  ;;  Create a cell containing @var{value}.
  ;;@end defun

  ;;@defun cell? thing
  ;;  Return @code{#t} if @var{thing} is a cell.
  ;;@end defun

  ;;@defun cell-ref cell
  ;; Return the contents of @var{cell}.
  ;;@end defun

  ;;@defun cell-set! cell value
  ;; Set the contents of @var{cell} to @var{value}.
  ;;@end defun

  (define-record-type cell
    (make-cell value)
    cell?
    (value cell-ref cell-set!)))

;;; cells.sls ends here
