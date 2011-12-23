;;; package.scm --- 

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

;; A `package' object records all metadata about a package, which
;; consists of:
;;
;; - The name and version
;; - The properties, of which the dependencies receive special attention
;; - The inventories (these are often not present, i.e. the empty (a)list)
;;
;; The `depends' property is available in parsed form via
;; `package-dependencies'; this returns a list of lists of
;; `dependency-choice's; of each group (list) of dependency choices in
;; the outer list there must be at least one that is satisfied for the
;; package to be in good state.

;;; Code:
#!r6rs

(library (guildhall package)
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
          package-synopsis
          package-description
          package-homepage
          
          package->string
          string->package
          maybe-string->package
          
          package-dependencies
          package-provides
          package-categories
          package-category-inventory
          package-inventories
          package-with-inventories
          package-modify-inventories
          
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

          version-constraint-satisfied?
          null-version-constraint?
          
          ;; these are mainly for the test suite
          form->dependency-choice
          form->version-constraint
          version-constraint->form)
  (import (rnrs)
          (only (srfi :1) every)
          (srfi :2 and-let*)
          (only (srfi :13)
                string-contains
                string-join
                string-skip)
          (srfi :14 char-sets)
          (srfi :67 compare-procedures)
          (guildhall ext foof-loop)
          (only (guildhall spells string-utils) string-split)
          (guildhall spells record-types)
          (only (guile) assq-ref and-map or-map and=>)
          (ice-9 match)
          (guildhall spells condition)
          (guildhall spells algebraic-types)
          (guildhall private utils)
          (guildhall inventory))

(define-record-type* package
  (make-package name
                version
                dependencies
                properties
                inventories)
  ())

(define-functional-fields package
  name version dependencies properties inventories)

(define (make-loser who)
  (lambda (primary message . irritants)
    (raise (condition
            primary
            (make-who-condition who)
            (make-message-condition message)
            (make-irritants-condition irritants)))))

(define construct-package
  (let ((lose (make-loser 'make-package))) ;use exported name
    (case-lambda
      ((name version properties inventories)
       (make-package name
                     (check-version version lose)
                     (%forms->dependencies
                      (or (assq-ref properties 'depends) '())
                      lose)
                     (validate-properties properties lose)
                     inventories))
      ((name version properties)
       (construct-package name version properties '()))
      ((name version)
       (construct-package name version '() '())))))

(define (package-description package)
  (package-property package 'description '()))

(define (package-synopsis package)
  (or (and=> (package-property package 'synopsis #f) car)
      ""))

(define (package-homepage package)
  (and=> (package-property package 'homepage #f) car))

(define (package-provides package)
  (package-property package 'provides '()))

(define (package=? p1 p2)
  (and (eq? (package-name p1)
            (package-name p2))
       (package-version=? (package-version p1)
                          (package-version p2))))

(define package-with-property
  (let ((lose (make-loser 'package-with-property)))
    (lambda (package property-name property-value)
      (validate-property property-name property-value lose)
      (make-package (package-name package)
                    (package-version package)
                    (if (eq? 'depends property-name)
                        (%forms->dependencies property-value lose)
                        (package-dependencies package))
                    (cons (cons property-name property-value)
                          (filter (lambda (property)
                                    (not (eq? (car property) property-name)))
                                  (package-properties package)))
                    (package-inventories package)))))

(define (package->string package separator)
  (let ((version (package-version package))
        (name-string (symbol->string (package-name package))))
    (string-append name-string separator (package-version->string version))))

(define (string->package string separator)
  (or (maybe-string->package string separator)
      (assertion-violation 'string->package
                           "cannot parse string"
                           string separator)))

(define (maybe-string->package string separator)
  (define (maybe-substring->name s start end)
    (if (string-skip s char-set:package-name start end)
        #f
        (string->symbol (substring s start end))))
  (and-let* ((index (string-contains string separator))
             (name (maybe-substring->name string 0 index))
             (version (maybe-string->package-version
                       (substring string
                                  (+ index (string-length separator))
                                  (string-length string)))))
    (construct-package name version)))

(define char-set:package-name
  (char-set-union char-set:letter+digit
                  (string->char-set "-")))

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
                     (make-irritants-condition (list form))
                     (if stacked
                         (list (make-stacked-condition stacked))
                         '()))))
     (match form
       (('package (name . version) . properties)
        (guard (c ((or (dependency-form-error? c)
                       (version-form-error? c)
                       (package-property-error? c))
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

(define (string->package-version string)
  (or (maybe-string->package-version string)
      (assertion-violation 'string->package-version
                           "invalid version string" string)))

(define (map/and proc list)
  (loop continue ((for item (in-list list))
                  (let result-item (proc item))
                  (for result (listing result-item)))
      => result
      (and result-item (continue))))

(define (maybe-string->package-version string)
  (map/and (lambda (part)
             (map/and (lambda (item)
                        (and (not (string-skip item char-set:digit))
                             (string->number item)))
                      (string-split part ".")))
           (string-split string "-")))

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
       (version-constraint-satisfied?
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

(define (version-constraint-satisfied? constraint version)
  (define (check-range lower upper lower-cmp upper-cmp)
    (cond ((and lower upper)
           (and (lower-cmp lower version)
                (upper-cmp version upper)))
          ((not lower)
           (upper-cmp version upper))
          ((not upper)
           (lower-cmp lower version))
          (else
           (assertion-violation 'version-constraint-satisfied?
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
     (not (version-constraint-satisfied? constraint version)))
    ((version-or constraints)
     (or-map (lambda (constraint)
               (version-constraint-satisfied? constraint version))
             constraints))
    ((version-and constraints)
     (and-map (lambda (constraint)
               (version-constraint-satisfied? constraint version))
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
    (cond ((and lower upper (package-version=? lower upper))
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


;;; Property value validation

(define (validate-properties properties lose)
  (loop ((for property (in-list properties)))
    => properties
    (validate-property (car property) (cdr property) lose)))

(define (list-of-predicate item-predicate)
  (lambda (thing)
    (and (list? thing)
         (for-all item-predicate thing))))

(define (singleton-list-predicate predicate)
  (lambda (thing)
    (and (pair? thing)
         (null? (cdr thing))
         (predicate (car thing)))))

(define property-validators
  `((description . ,(list-of-predicate string?))
    (synopsis . ,(singleton-list-predicate string?))
    (homepage . ,(singleton-list-predicate string?))
    (provides . ,(list-of-predicate symbol?))))

(define (validate-property name value lose)
  (cond ((assq-ref property-validators name)
         => (lambda (valid?)
              (or (valid? value)
                  (lose (make-package-property-error name value)
                        "invalid value for property"
                        name value))))
        (else
         (values))))


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

(define-condition-type &package-property &error
  make-package-property-error package-property-error?
  (name package-property-error-name)
  (value package-property-error-value))

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
