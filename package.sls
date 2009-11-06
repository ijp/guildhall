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
  (export (rename (construct-package make-package))
          package?
          package=?
          package-name

          package-version
          package-modify-version
          package-with-version
          package-version-string

          package-property
          package-with-property
          package-properties
          
          package-identifier
          package-dependencies
          package-categories
          package-category-inventory
          package-inventories
          package-with-inventories
          package-identifier->package
          
          package->form
          parse-package-form

          package-version?
          package-version=?
          package-version<?
          package-version<=?
          package-version>?
          package-version>=?
          package-version-compare
          string->package-version
          package-version->string

          dependency-choice?
          dependency-choice-target
          dependency-choice-version-constraint
          dependency-choice-satisfied?

          package-form-error?
          package-error-form
          
          ;; these are mainly for the test suite
          form->dependency-choice
          form->version-constraint
          version-constraint->form)
  (import (rnrs)
          (only (srfi :1) every)
          (only (srfi :13) string-join)
          (srfi :67 compare-procedures)
          (only (spells string-utils) string-split)
          (spells record-types)
          (spells foof-loop)
          (spells irregex)
          (spells alist)
          (spells misc)
          (spells match)
          (spells condition)
          (spells algebraic-types)
          (spells tracing) ;++debug
          (dorodango private utils)
          (dorodango inventory))

(define-record-type* package
  (make-package name
                version
                dependencies
                properties
                inventories)
  ())

(define-functional-fields package
  name version dependencies properties inventories)

(define construct-package
  (case-lambda
    ((name version properties inventories)
     (let ((lose (make-loser 'make-package))) ;use exported name
       (make-package name
                     (check-version version lose)
                     (%forms->dependencies
                      (or (assq-ref properties 'depends) '())
                      lose)
                     properties
                     inventories)))
    ((name version properties)
     (construct-package name version properties '()))
    ((name version)
     (construct-package name version '() '()))))

(define (package=? p1 p2)
  (and (eq? (package-name p1)
            (package-name p2))
       (package-version=? (package-version p1)
                          (package-version p2))))

(define (package-with-property package property-name property-value)
  (make-package (package-name package)
                (package-version package)
                (if (eq? 'depends property-name)
                    (%forms->dependencies property-value
                                          (make-loser 'package-with-property))
                    (package-dependencies package))
                (cons (cons property-name property-value)
                      (package-properties package))
                (package-inventories package)))

(define (package-identifier package)
  (let ((version (package-version package))
        (name-string (symbol->string (package-name package))))
    (if (null? version)
        name-string
        (string-append name-string "-" (package-version->string version)))))

(define package-identifier->package
  (let* ((sub-version-sre '(: (* (+ numeric) ".") (+ numeric)))
         (identifier-irx (irregex `(: (=> name  (*? any)) "-"
                                      (=> version (* (: ,sub-version-sre "-"))
                                          ,sub-version-sre)
                                      eos))))
    (lambda (identifier)
      (cond ((irregex-search identifier-irx identifier)
             => (lambda (match)
                  (construct-package (string->symbol
                                      (irregex-match-substring match 'name))
                                     (string->package-version
                                      (irregex-match-substring match 'version)))))
            (else
             #f)))))

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
                     (make-message-condition  "invalid package form")
                     (make-irritants-condition form)
                     (if stacked
                         (list (make-stacked-condition stacked))
                         '()))))
     (match form
       (('package (name . version) . properties)
        (guard (c ((or (dependency-form-error? c)
                       (version-form-error? c))
                   (lose c)))
          (construct-package name
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
  (list-compare sub-version-compare v1 v2))

(define (sub-version-compare sub1 sub2)
  (list-compare integer-compare sub1 sub2))

(define package-version=? (=? package-version-compare))
(define package-version<? (<? package-version-compare))
(define package-version<=? (<=? package-version-compare))
(define package-version>? (>? package-version-compare))
(define package-version>=? (>=? package-version-compare))

(define (package-version->string version)
  (string-join (map (lambda (part)
                      (string-join (map number->string part) "."))
                    version)
               "-"))

(define (string->package-version s)
  (map (lambda (part)
         (map (lambda (item)
                (let ((n (string->number item)))
                  (unless (and n (integer? n))
                    (assertion-violation 'string->package-version
                                         "invalid version string" s))
                  n))
              (string-split part ".")))
       (string-split s "-")))

(define (package-version? thing)
  (list-of sub-version? thing))

(define (sub-version? thing)
  (list-of integer? thing))

(define (list-of predicate thing)
  (and (pair? thing)
       (predicate (car thing))
       (let ((rest (cdr thing)))
         (or (null? rest)
             (list-of predicate rest)))))

(define (->package-version thing lose)
  (if (package-version? thing)
      thing
      (lose (make-version-form-error thing)
            "not a valid package version"
            thing)))

(define (->package-version/sloppy thing lose)
  (->package-version (if (sub-version? thing)
                         (list thing)
                         thing)
                     lose))


;;; Package dependencies

(define-record-type dependency-choice
  (fields target version-constraint))

(define (dependency-choice-satisfied? choice package)
  (and (eq? (dependency-choice-target choice)
            (package-name package))
       (version-constraint-satisified?
        (dependency-choice-version-constraint choice)
        (package-version package))))

(define-datatype <version-constraint>
  (version-range (lower upper))
  (version-:range (lower upper))
  (version-range: (lower upper))
  (version-:range: (lower upper))
  (version-not (constraint))
  (version-or (constraints))
  (version-and (constraints)))

(define (null-version-constraint? constraint)
  (cases <version-constraint> constraint
    ((version-and constraints) (null? constraints))
    (else                      #f)))

(define (version-constraint-satisified? constraint version)
  (define (check-range lower upper lower-cmp upper-cmp)
    (cond ((and lower upper)
           (and (lower-cmp lower version)
                (upper-cmp version upper)))
          ((not lower)
           (upper-cmp version upper))
          ((not upper)
           (lower-cmp lower version))
          (else
           (assertion-violation 'version-constraint-satisified?
                                "invalid range constraint encountered"))))
  (cases <version-constraint> constraint
    ((version-range lower upper)
     (check-range lower upper package-version<? package-version<?))
    ((version-:range lower upper)
     (check-range lower upper package-version<=? package-version<?))
    ((version-range: lower upper)
     (check-range lower upper package-version<? package-version<=?))
    ((version-:range: lower upper)
     (check-range lower upper package-version<=? package-version<=?))
    ((version-not constraint)
     (not (version-constraint-satisified? constraint version)))
    ((version-or constraints)
     (or-map (lambda (constraint)
               (version-constraint-satisified? constraint version))
             constraints))
    ((version-and constraints)
     (and-map (lambda (constraint)
               (version-constraint-satisified? constraint version))
             constraints))))

(define (%forms->dependencies forms lose)
  (map (lambda (form)
         (%form->dependency form lose))
       forms))

(define (%form->dependency form lose)
  (match form
    (('or choices ___)
     (map (lambda (choice-form)
            (%form->dependency-choice choice-form lose))
          choices))
    (_
     (list (%form->dependency-choice form lose)))))

(define (form->dependency-choice form)
  (%form->dependency-choice form (make-loser 'form->dependency-choice)))

(define (%form->dependency-choice form lose)
  (match form
    ((name)
     (make-dependency-choice name (make-version-and '())))
    ((name sub-constraints ___)
     (make-dependency-choice name (%form->constraint sub-constraints lose)))
    (_
     (lose (make-dependency-form-error form)
           "invalid dependency form"
           form))))

(define (form->version-constraint form)
  (%form->constraint form (make-loser 'form->version-constraint)))

(define (%form->constraint form lose)
  (match form
    (('<= version ___)
     (make-version-:range: #f (->package-version version lose)))
    (('>= version ___)
     (make-version-:range: (->package-version version lose) #f))
    (('< version ___)
     (make-version-:range #f (->package-version version lose)))
    (('> version ___)
     (make-version-range: (->package-version version lose) #f ))
    (('not sub-form)
     (make-version-not (%form->constraint sub-form lose)))
    (('or sub-forms ___)
     (make-version-or (map (lambda (sub-form)
                                 (%form->constraint sub-form lose))
                               sub-forms)))
    (('and sub-forms ___)
     (make-version-and (map (lambda (sub-form)
                                  (%form->constraint sub-form lose))
                                sub-forms)))
    ((((? symbol? head) . rest))
     (%form->constraint (car form) lose))
    (version
     (let ((version (->package-version/sloppy version lose)))
       (make-version-:range: version version)))))

(define (version-constraint->form constraint)
  (define (range-form lower-check lower upper-check upper)
    (cond ((equal? lower upper)
           lower)
          ((not upper)
           `(,lower-check ,@lower))
          ((not lower)
           `(,upper-check ,@upper))
          (else
           (assertion-violation 'version-constraint->form
                                "unsupported range encountered" lower upper))))
  (cases <version-constraint> constraint
    ((version-range start end)
     (range-form '> start '< end))
    ((version-:range start end)
     (range-form '>= start '< end))
    ((version-range: start end)
     (range-form '> start '<= end))
    ((version-:range: start end)
     (range-form '>= start '<= end))
    ((version-not sub-constraint)
     `(not ,(version-constraint->form sub-constraint)))
    ((version-or sub-constraints)
     `(or ,@(map version-constraint->form sub-constraints)))
    ((version-and sub-constraints)
     `(and ,@(map version-constraint->form sub-constraints)))))


;;; Conditions

(define-condition-type &dependency-form &error
  make-dependency-form-error dependency-form-error?
  (form dependency-error-form))

(define-condition-type &version-form &error
  make-version-form-error version-form-error?
  (form version-error-form))

(define-condition-type &package-form &error
  make-package-form-error package-form-error?
  (form package-error-form))

(define (make-loser who)
  (lambda (primary message . irritants)
    (raise (condition
            primary
            (make-who-condition who)
            (make-message-condition message)
            (make-irritants-condition irritants)))))

(define (check-version version lose)
  (if (package-version? version)
      version
      (lose (make-version-form-error version)
            "not a valid package version"
            version)))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1) (cases 2))
;; End:
