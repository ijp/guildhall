;;; solver.sls --- Dependency solver, algorithm

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

(library (dorodango solver)
  (export make-solver
          solver?
          find-next-solution!

          solution?)
  (import (rnrs)
          (spells record-types)
          (spells logging)
          (spells tracing)
          (dorodango private utils)
          (dorodango solver internals))

(define-record-type* solver
  (really-make-solver universe)
  ())

(define-record-type* solution
  (make-solution)
  ())

(define default-options
  `((step-score . 10)
    (broken-score . 10)
    (infinity . 1000)
    (gloal-score . 10)
    (future-horizon . 50)))

(define make-solver
  (case-lambda
    ((universe options)
     (really-make-solver universe))
    ((universe)
     (make-solver universe default-options))))

(define (find-next-solution! solver max-steps)
  #f)

(define logger:dorodango.solver (make-logger logger:dorodango 'solver))
(define log/debug (make-fmt-log logger:dorodango 'debug))

)
