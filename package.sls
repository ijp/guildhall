;;; package.sls --- 

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

(library (dorodango package)
  (export make-package
          package?
          package=?
          package-name
          package-version
          package-property
          package-identifier
          package-version-string
          package-categories
          package-category-inventory
          package-inventories
          package-with-inventories
          
          package->form
          parse-package-form

          package-version=?
          package-version<?
          package-version>?
          package-version-compare)
  (import (rnrs)
          (only (srfi :13) string-join)
          (srfi :67 compare-procedures)
          (only (spells record-types)
                define-functional-fields)
          (spells alist)
          (spells match)
          (dorodango inventory))

(define-record-type package
  (protocol (lambda (p)
              (case-lambda
                ((name version properties inventories)
                 (p name version properties inventories))
                ((name version properties)
                 (p name version properties '()))
                ((name version)
                 (p name version '() '())))))
  (fields name version properties inventories))

(define (package=? p1 p2)
  (and (eq? (package-name p1)
            (package-name p2))
       (package-version=? (package-version p1)
                          (package-version p2))))

(define-functional-fields package
  name version properties inventories)

(define (package-identifier package)
  (let ((version (package-version package))
        (name-string (symbol->string (package-name package))))
    (if (null? version)
        name-string
        (string-append name-string "-" (version->string version)))))

(define (package-version-string package)
  (version->string (package-version package)))

(define (package-property package property default)
  (cond ((assq property (package-properties package))
         => cdr)
        (else
         default)))

(define (version->string version)
  (string-join (map (lambda (part)
                      (string-join part "."))
                    version)
               "-"))

(define (package-categories package)
  (map inventory-name (package-inventories package)))

(define (package-category-inventory package category)
  (cond ((memp (lambda (inventory)
                 (eq? category (inventory-name inventory)))
               (package-inventories package))
         => car)
        (else #f)))

(define parse-package-form
  (case-lambda
    ((form make-categories)
     (match form
       (('package (name . version) . properties)
        (make-package name
                      version
                      properties
                      (make-categories properties)))
       (_
        #f)))
    ((form)
     (parse-package-form form (lambda (properties) '())))))

(define (package->form package)
  `(package (,(package-name package) . ,(package-version package))
            . ,(package-properties package)))

(define (package-version-compare v1 v2)
  (list-compare (lambda (p1 p2)
                  (list-compare integer-compare p1 p2))
                v1
                v2))

(define package-version=? (=? package-version-compare))
(define package-version<? (<? package-version-compare))
(define package-version>? (>? package-version-compare))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:
