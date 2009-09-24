;;; logging.sls --- Dependency solver, logging utilities

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (dorodango solver logging)
  (export logger:dorodango.solver
          log/trace
          log/debug
          log/info

          internal-error)
  (import (rnrs base)
          (spells logging)
          (spells fmt)
          (dorodango private utils))

(define logger:dorodango.solver (make-logger logger:dorodango 'solver))
(define log/info (make-fmt-log logger:dorodango 'info))
(define log/debug (make-fmt-log logger:dorodango 'debug))
(define log/trace (make-fmt-log logger:dorodango 'trace))

(define (internal-error . formats)
  (assertion-violation 'dorodango.solver
                       (fmt #f (cat "Internal error: " (apply-cat formats)))))

)
