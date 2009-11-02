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
          package-dependencies
          package-categories
          package-category-inventory
          package-inventories
          package-with-inventories
          
          package->form
          parse-package-form

          package-version=?
          package-version<?
          package-version>?
          package-version-compare

          dependency-choice?
          dependency-choice-target
          dependency-choice-version-constraints

          version-constraint
          version-between
          version-or
          version-and)
  (import (rnrs)
          (only (srfi :13) string-join)
          (srfi :67 compare-procedures)
          (spells record-types)
          (spells alist)
          (spells match)
          (spells condition)
          (spells algebraic-types)
          (dorodango inventory))

(define-record-type* package
  (%make-package name
                 version
                 dependencies
                 properties
                 inventories)
  ())

(define make-package
  (let ((lose (lambda (primary message . irritants)
                (raise (condition
                        primary
                        (make-who-condition 'make-package)
                        (make-message-condition message)
                        (make-irritants-condition irritants))))))
    (case-lambda
      ((name version properties inventories)
       (%make-package name
                      version
                      (forms->dependencies
                       (or (assq-ref properties 'depends) '()) lose)
                      properties
                      inventories))
      ((name version properties)
       (make-package name version properties '()))
      ((name version)
       (make-package name version '() '())))))

(define (package=? p1 p2)
  (and (eq? (package-name p1)
            (package-name p2))
       (package-version=? (package-version p1)
                          (package-version p2))))

(define (package-with-inventories package inventories)
  (%make-package (package-name package)
                 (package-version package)
                 (package-properties package)
                 (package-dependencies package)
                 inventories))

(define (package-identifier package)
  (let ((version (package-version package))
        (name-string (symbol->string (package-name package))))
    (if (null? version)
        name-string
        (string-append name-string "-" (package-version->string version)))))

(define (package-version-string package)
  (package-version->string (package-version package)))

(define (package-property package property default)
  (cond ((assq property (package-properties package))
         => cdr)
        (else
         default)))

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
     (define (lose stacked)
       (raise (apply condition
                     (make-package-form-error form)
                     (make-who-condition 'parse-package-form)
                     (make-message-condition  "invalid rule form")
                     (make-irritants-condition form)
                     (if stacked
                         (list (make-stacked-condition stacked))
                         '()))))
     (match form
       (('package (name . version) . properties)
        (guard (c ((dependency-form-error? c)
                   (lose c)))
          (make-package name
                        version
                        properties
                        (make-categories properties))))
       (_
        (lose #f))))
    ((form)
     (parse-package-form form (lambda (properties) '())))))

(define (package->form package)
  `(package (,(package-name package) . ,(package-version package))
            . ,(package-properties package)))


;;; Package versions

(define (package-version-compare v1 v2)
  (list-compare (lambda (p1 p2)
                  (list-compare integer-compare p1 p2))
                v1
                v2))

(define package-version=? (=? package-version-compare))
(define package-version<? (<? package-version-compare))
(define package-version>? (>? package-version-compare))

(define (package-version->string version)
  (string-join (map (lambda (part)
                      (string-join part "."))
                    version)
               "-"))


;;; Package dependencies

(define-record-type dependency-choice
  (fields target version-constraints))

(define-datatype version-constraint
  (version-between (lower upper))
  (version-or (constraints))
  (version-and (constraints)))

(define (forms->dependencies forms lose)
  (map (lambda (form)
         (form->dependency form lose))
       forms))

(define (form->dependency form lose)
  (match form
    (('or choices ___)
     (map (lambda (choice-form)
            (form->dependency-choice choice-form lose))
          choices))
    (_
     (list (form->dependency-choice form lose)))))

(define (form->dependency-choice form lose)
  (match form
    ((name)
     (make-dependency-choice name '()))
    ((name constraint-form)
     (make-dependency-choice name (form->constraint constraint-form lose)))
    (_
     (lose (make-dependency-form-error form)
           "invalid dependency form"
           form))))

(define (form->constraint form lose)
  (match form
    (('<= version)
     (make-version-between ))))

;;; Conditions

(define-condition-type &dependency-form &error
  make-dependency-form-error dependency-form-error?
  (form dependency-error-form))

(define-condition-type &package-form &error
  make-package-form-error package-form-error?
  (form package-error-form))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:
