;;; bundle.scm --- Bundle unit tests

;; Copyright (C) 2011 Free Software Foundation, Inc.
;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

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
        (only (srfi :1) append-map)
        (guildhall ext trc-testing)
        (guildhall spells pathname)
        (only (guile) getenv)
        (guildhall package)
        (guildhall bundle))

(define (this-directory) (or (getenv "srcdir") "."))


(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (sorted-package-names packages)
  (list-sort symbol<? (map package-name packages)))


(define-test-suite bundle-tests
  "Bundles")

(define expected-package-map
  '((("bar") bar)
    (("file-conflict-foo") file-conflict-foo)
    (("foo") foo)
    (("hook") hook hook-crash hook-source-needed)
    (("multi") multi-core multi-tools)
    (("unsatisfied-depends") unsatisfied-depends)))

(define (test-bundle-contents bundle)
  (test-eqv #t (bundle? bundle))
  (test-equal (append-map cdr expected-package-map)
    (sorted-package-names (bundle-packages bundle)))
  (test-equal expected-package-map
    (list-sort (lambda (entry-1 entry-2)
                 (string<? (caar entry-1) (caar entry-2)))
               (map (lambda (entry)
                      (cons (car entry) (sorted-package-names (cdr entry))))
                    (bundle-package-map bundle)))))

(define-test-case bundle-tests open/directory ()
  (let ((bundle (open-input-bundle (pathname-join (this-directory) "bundle"))))
    (test-bundle-contents bundle)))

(define-test-case bundle-tests open/zip ()
  (let ((bundle (open-input-bundle (pathname-join (this-directory) "bundle.zip"))))
    (test-bundle-contents bundle)))

(exit (run-test-suite bundle-tests))

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
