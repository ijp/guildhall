;;; package.scm --- Test suite for `package.sls'

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(import (rnrs)
        (only (srfi :1) append-map)
        (wak trc-testing)
        (dorodango package))

(define-test-suite package-tests
  "Package datastructures")


;;; Dependency stuff

(define-test-suite (package-tests.dependencies package-tests)
  "Package dependencies")

(define-test-case package-tests.dependencies roundtrip ()
  (for-each (lambda (constraint)
              (test-equal constraint
                (version-constraint->form
                 (form->version-constraint constraint))))
            '((or (and (>= (1)) (not (>= (2)))) (>= (2 1)))
              (>= (1))
              ((2 3)))))

(define-syntax test-satisfied
  (syntax-rules ()
    ((_ value form package-name version)
     (test-eqv value
       (dependency-choice-satisfied?
        (form->dependency-choice form)
        (make-package package-name version))))))

(define-test-case package-tests.dependencies satisfaction ()
  (test-satisfied #t '(spells)
    'spells '((0 1)))
  (test-satisfied #f '(spells)
    'srfi '((0 1)))
  (test-satisfied #t '(spells (not (0)))
    'spells '((1 2)))
  (test-satisfied #f '(spells (not (0)))
    'spells '((0)))
  (test-satisfied #t '(spells (>= (1)))
    'spells '((1 2)))
  (test-satisfied #f '(spells (>= (1)))
    'spells '((0 9)))
  (test-satisfied #t '(spells (and (>= (1)) (<= (2))))
    'spells '((1 1)))
  (test-satisfied #f '(spells (and (>= (1)) (<= (2))))
    'spells '((2 1)))
  (test-satisfied #t '(spells (and (> (1)) (<= (2))))
    'spells '((2)))
  (test-satisfied #f '(spells (and (> (1)) (<= (2))))
    'spells '((2 1))))


;;; Form parsing

(define-test-suite (package-tests.parsing package-tests)
  "Parsing package forms")

(define-test-case package-tests.parsing version ()
  (test-compare package=? (make-package 'foo '((0 1)))
    (parse-package-form '(package (foo (0 1))))))

(define-test-case package-tests.parsing properties ()
  (let ((package (parse-package-form '(package (foo (0 1))
                                               (bar "1")
                                               (baz 2)))))
    (test-compare package=? (make-package 'foo '((0 1)))
      package)
    (test-equal '((bar "1") (baz 2))
      (package-properties package))))

(define-test-case package-tests.parsing dependencies ()
  (let ((package (parse-package-form '(package (foo (0 1))
                                               (depends (bar) (baz))))))
    (test-equal '(bar baz)
      (append-map (lambda (dependency)
                    (map dependency-choice-target dependency))
                  (package-dependencies package)))))

(define-test-case package-tests.parsing failure ()
  (test-equal '(form-error (package (foo 0 1)))
    (guard (c ((package-form-error? c)
               `(form-error ,(package-error-form c))))
      (parse-package-form '(package (foo 0 1))))))

(define-test-case package-tests.parsing roundtrip ()
  (for-each
   (lambda (package-form)
     (test-equal package-form
       (package->form (parse-package-form package-form))))
   '((package (foo (0 1))
              (depends (bar (0 1))
                       (or (foo) (frobotz (and (>= (1 1)) (< (2))))))))))


(run-test-suite package-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (test-satisfied 2))
;; End:
