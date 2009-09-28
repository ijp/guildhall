;;; expression.scm --- Tests for the expression DAG

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

(import (rnrs)
        (spells testing)
        (dorodango solver expression))

(define-test-suite expression-tests
  "Dynamically updateable expression DAG")

(define-test-case expression-tests simple-bool ()
  (let* ((v1 (make-var-expression #t eqv?))
         (v2 (make-var-expression #f eqv?))
         (v1-and-v2 (make-and-expression (list v1 v2)))
         (v1-or-v2 (make-or-expression (list v1 v2))))
    ;; check initial values
    (test-eqv #t (expression/value v1))
    (test-eqv #f (expression/value v2))
    (test-eqv #f (expression/value v1-and-v2))
    (test-eqv #t (expression/value v1-or-v2))

    ;; check propagation
    (expression/set-value! v1 #f)
    (test-eqv #f (expression/value v1-or-v2))

    (expression/set-value! v1 #t)
    (expression/set-value! v2 #t)
    (test-eqv #t (expression/value v1-and-v2))))

(run-test-suite expression-tests)
