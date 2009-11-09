;;; cmdline.sls --- Command-line UI library

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

(library (dorodango ui cmdline)
  (export apply-actions

          message
          prompt
          
          dsp-bundle
          
          dsp-db-item
          dsp-db-item/short
          dsp-db-item-identifier)
  (import (rnrs)
          (srfi :8 receive)
          (only (srfi :13) string-null? string-trim-both)
          (srfi :67 compare-procedures)
          (spells fmt)
          (spells match)
          (spells foof-loop)
          (spells xvector)
          (spells finite-types)
          (spells tracing)
          (only (spells record-types) define-record-type*)
          (dorodango private utils)
          (dorodango inventory)
          (dorodango package)
          (dorodango bundle)
          (dorodango database)
          (dorodango database dependencies)
          (dorodango solver)
          (dorodango solver choice)
          (prefix (dorodango solver universe)
                  universe-))


;;; Dependency management

(define (apply-actions db to-install to-remove)
  (receive (universe package-table) (database->universe db)
    (let ((irreparable (irreparable-packages universe)))
      (cond ((null? irreparable)
             (let ((result (resolve-dependencies universe
                                                 package-table
                                                 to-install
                                                 to-remove)))
               (cond ((choice-set? result)
                      (apply-choices db result))
                     (else
                      result))))
            ((y-or-n #f
                     (cat (wrap-lines
                           "The following packages have unsatisfiable dependencies"
                           " and must be removed before proceeding:")
                          (wrap-lines
                           (fmt-join dsp-package-name irreparable " ")) "\n"
                          "Remove these packages and proceed?"))
             (for-each (lambda (package)
                         (database-remove! db package)))
             (apply-actions db to-install to-remove))
            (else
             #f)))))

(define (apply-choices db choices)
  (loop ((for choice (in-choice-set choices)))
    (let ((version (choice-version choice)))
      (cond ((universe-version-tag version)
             (database-install! db (universe-version->package version)))
            (else
             (database-remove! db (universe-version-package-name version)))))))

(define (resolve-dependencies universe package-table to-install to-remove)
  (receive (initial-choices version-scores)
           (solver-options package-table to-install to-remove)
    (define (solution->actions solution)
      (choice-set-union initial-choices (solution-choices solution)))
    (let ((solver (make-solver universe
                               `((version-scores . ,version-scores)
                                 (initial-choices . ,initial-choices))))
          (solutions (make-xvector))
          (max-steps 5000))
      (define (prompt-choices index exhausted?)
        (append '((#\y "Accept this solution"))
                (if exhausted? '() '((#\n "Try to find another solution")))
                '((#\q "Quit the operation"))
                (if (< index (- (xvector-length solutions) 1))
                    '((#\. "Select next solution"))
                    '())
                (if (> index 0)
                    '((#\, "Select previous solution"))
                    '())))
      (loop continue ((with selected-index #f)
                      (with exhausted? #f))
        (define (solution-prompt now-exhausted? index)
          (define (iterate new-index)
            (continue (=> selected-index new-index)
                      (=> exhausted? (or exhausted? now-exhausted?))))
          (let*-values (((solution) (xvector-ref solutions index))
                        ((risky? message) (assess-solution solution
                                                           initial-choices
                                                           package-table)))
            (cond
              (risky?
               (case (prompt
                      (if now-exhausted?
                          (cat "No more solutions. Proceed with previous solution?")
                          (cat message "Accept this solution?"))
                      (prompt-choices index (or exhausted? now-exhausted?)))
                 ((#\y)       (solution->actions (xvector-ref solutions index)))
                 ((#\n)       (iterate #f))
                 ((#\q)       #f)
                 ((#\.)       (iterate (+ index 1)))
                 ((#\,)       (iterate (- index 1)))
                 (else
                  (assert #f))))
              ((y-or-n #t (cat message "Do you want to continue?"))
               (solution->actions solution))
              (else
               #f))))
        (cond (selected-index
               (solution-prompt #f selected-index))
              ((and (not exhausted?)
                    (find-next-solution! solver max-steps))
               => (lambda (solution)
                    (xvector-push! solutions solution)
                    (solution-prompt #f (- (xvector-length solutions) 1))))
              ((> (xvector-length solutions) 0)
               (solution-prompt (not exhausted?) (- (xvector-length solutions) 1)))
              (else
               (message "Unable to resolve dependencies! Giving up...")
               #f))))))

;; The action that will be taken on a package; it contains two
;; components: what actually will happen with the package (`type'),
;; and how that relates to the user's request (`compliance').
(define-record-type* package-action
  (make-package-action type compliance)
  ())

(define-enumerated-type action-type <action-type>
  action-type?
  action-types
  action-type-name
  action-type-index
  (install remove upgrade keep downgrade))

(define-enumerated-type action-compliance <action-compliance>
  action-compliance?
  action-compliances
  action-compliance-name
  action-compliance-index
  (ordered auto disobedient already))

(define (package-action-type-index action)
  (action-type-index (package-action-type action)))

(define (package-action-compliance-index action)
  (action-compliance-index (package-action-compliance action)))

(define (package-action-ordered? action)
  (eq? (package-action-compliance action) (action-compliance ordered)))

(define (package-action-disobedient? action)
  (eq? (package-action-compliance action) (action-compliance disobedient)))

(define-syntax make-finite-type-vector
  (syntax-rules ()
    ((_ constructor instances index (instance value) ...)
     (let ((result (make-vector (vector-length instances))))
       (vector-set! result (index (constructor instance)) value)
       ...
       result))))

(define action-type-headings
  (make-finite-type-vector action-type action-types action-type-index
    (install "The following NEW packages will be installed:")
    (remove "The following packages will be REMOVED:")
    (upgrade "The following packages will be upgraded:")
    (keep "The following packages have been kept back:")
    (downgrade "The following packages will be downgraded:")))

(define action-compliance-decorators
  (make-finite-type-vector action-compliance
                           action-compliances
                           action-compliance-index
    (ordered cat)
    (auto (lambda (name) (cat name "{a}")))
    (disobedient (lambda (name) (cat name "{!}")))
    (already (lambda (name) (cat name "{=}")))))

(define (compute-action current-version version to-install? to-remove?)
  (cond ((and (not current-version) version)
         (make-package-action
          (action-type install)
          (cond (to-install?  (action-compliance ordered))
                (to-remove?   (action-compliance disobedient))
                (else         (action-compliance auto)))))
        ((and current-version (not version))
         (make-package-action
          (action-type remove)
          (cond (to-remove?   (action-compliance ordered))
                (to-install?  (action-compliance disobedient))
                (else         (action-compliance auto)))))
        ((and current-version version)
         (make-package-action
          (if3 (package-version-compare current-version version)
               (action-type upgrade)
               (action-type keep)
               (action-type downgrade))
          (cond (to-remove? (action-compliance disobedient))
                (to-install? (action-compliance already))
                (else        (action-compliance auto)))))))

(define (assess-solution solution initial-choices package-table)
  (define (package-name.action<? p1 p2)
    (symbol<? (car p1) (car p2)))
  (let ((action-lists (make-vector (vector-length action-types) '())))
    (define (accumulate-message)
      (loop ((for type (in-vector action-types))
             (for action-heading (in-vector action-type-headings))
             (let action-list (vector-ref action-lists (action-type-index type)))
             (for message-parts (listing (cat action-heading "\n"
                                              (dsp-action-list action-list))
                                         (if (not (null? action-list))))))
        => (fmt-join dsp message-parts "\n")))
    (loop continue
        ((for choice (in-choice-set (choice-set-union
                                     initial-choices
                                     (solution-choices solution))))
         (with risky? #f))
      => (loop ((for type (in-vector action-types)))
           => (values risky? (accumulate-message))
           (let ((type-index (action-type-index type)))
             (vector-set! action-lists
                          type-index
                          (list-sort package-name.action<?
                                     (vector-ref action-lists type-index)))))
      (let* ((chosen-version (choice-set-version-of
                             initial-choices
                             (universe-version-package (choice-version choice))))
             (package-name (universe-version-package-name (choice-version choice)))
             (current-version
              (universe-package-current-version
               (hashtable-ref package-table package-name #f)))
             (action (compute-action (universe-version-tag current-version)
                                     (universe-version-tag (choice-version choice))
                                     (and chosen-version
                                          (universe-version-tag chosen-version)
                                          #t)
                                     (and chosen-version
                                          (not (universe-version-tag chosen-version)))))
             (type-index (package-action-type-index action)))
        (vector-set! action-lists
                     type-index
                     (cons (cons package-name action)
                           (vector-ref action-lists type-index)))
        (continue (=> risky? (or risky?
                                 (package-action-disobedient? action)
                                 (and (eq? (package-action-type action)
                                           (action-type remove))
                                      (not (package-action-ordered? action))))))))))


(define (dsp-action-list actions)
  (lambda (st)
    ((wrap-lines
       (fmt-join dsp
                 (loop ((for package.action (in-list actions))
                        (for formats
                             (listing ((vector-ref action-compliance-decorators
                                                   (package-action-compliance-index
                                                    (cdr package.action)))
                                       (car package.action)))))
                   => formats)
                 " ")) st)))

;; Find the universe version corresponding to a package
(define (package-table-lookup table package-name desired-version)
  (let ((universe-package (hashtable-ref table package-name #f))
        (matching-version?
         (if desired-version
             (lambda (version)
               (package-version=? (universe-version-tag version)
                                  desired-version))
             (lambda (version)
               (not (universe-version-tag version))))))
    (or (and universe-package
             (find matching-version? (universe-package-versions universe-package)))
        (assertion-violation 'package-table-lookup
                             "unable to find requested package"
                             package-name desired-version))))

(define (solver-options package-table to-install to-remove)
  (define (accumulate choices version-scores items find-version)
    (loop ((for item (in-list items))
           (let version (find-version item))
           (for version-scores (listing-reverse (initial version-scores)
                                                (cons version 10000)))
           (with choices
                 choices
                 (choice-set-insert-or-narrow choices
                                              (make-install-choice version #f))))
          => (values choices version-scores)))
  (define (package->version package)
    (package-table-lookup package-table
                          (package-name package)
                          (package-version package)))
  (define (package-name->uninstalled-version package-name)
    (package-table-lookup package-table package-name #f))
  (receive (choices version-scores)
           (accumulate (make-choice-set)
                       '()
                       to-install
                       package->version)
    (accumulate choices
                version-scores
                to-remove
                package-name->uninstalled-version)))

(define (universe-version->package version)
  (let ((universe-package (universe-version-package version)))
    (make-package (universe-package-name universe-package)
                  (universe-version-tag version))))

(define (universe-version-package-name version)
  (universe-package-name (universe-version-package version)))


;;; UI helpers

(define (fmt/stdout . formats)
  (fmt #t (apply-cat formats))
  (flush-output-port (current-output-port)))

(define (message . formats)
  (fmt/stdout (cat (apply-cat formats) nl)))

(define (y-or-n default message)
  (loop prompt-again ()
    (fmt/stdout (cat message " [" (if (eqv? default #t) "Y/n" "y/N") "] "))
    (let ((input (string-downcase (string-trim-both
                                   (get-line (current-input-port))))))
      (if (string-null? input)
          default
          (case (string-ref input 0)
            ((#\y) #t)
            ((#\n) #f)
            (else
             (fmt/stdout (cat "Please answer 'y' or 'n'.\n"))
             (prompt-again)))))))

(define (prompt message choices)
  (define (lookup-key keys input)
    (find (lambda (key)
            (cond ((char? key)
                   (char=? key (string-ref input 0)))
                  ((string? key)
                   (string=? key input))
                  (else
                   (assertion-violation 'prompt "Invalid key type " key))))
          keys))
  (loop ((for choice (in-list choices))
         (let-values (key help)
           (values (car choice) (cadr choice)))
         (for keys (listing key))
         (for help-texts (listing (cat key ": " help))))
    => (loop prompt-again ()
         (fmt/stdout (cat message " ["
                      (char-upcase (car keys)) "/" (fmt-join dsp (cdr keys) "/")
                      "/?] "))
         (let ((input (string-downcase (string-trim-both
                                        (get-line (current-input-port))))))
           (cond ((string-null? input)
                  (car keys))
                 ((string=? "?" input)
                  (fmt/stdout
                   (cat "The following choices are available:\n"
                        (fmt-join/suffix (lambda (text) (cat "  " text))
                                         help-texts
                                         "\n")))
                  (prompt-again))
                 ((lookup-key keys input) => values)
                 (else
                  (fmt/stdout
                   (cat "Invalid response. Please enter a valid choice"
                        " or '?' for help.\n"))
                  (prompt-again)))))))


;;; Formatting combinators

(define (dsp-package pkg)
  (cat "Package: " (package-name pkg) "\n"
       (cat "Version: " (dsp-package-version (package-version pkg)) "\n")
       (cat "Depends: "
            (fmt-join wrt (package-property pkg 'depends '()) ", ") "\n")
       (fmt-join (lambda (category)
                   (let ((inventory (package-category-inventory pkg category)))
                     (if (inventory-empty? inventory)
                         fmt-null
                         (cat "Inventory: " category "\n"
                              (dsp-inventory inventory)))))
                 (package-categories pkg))))

(define (dsp-db-item item)
  (dsp-package (database-item-package item)))

(define (dsp-inventory inventory)
  (define (dsp-node node path)
    (lambda (state)
      (loop next ((for cursor (in-inventory node))
                  (with st state))
        => st
        (let ((path (cons (inventory-name cursor) path)))
          (if (inventory-leaf? cursor)
              (next (=> st ((cat " " (fmt-join dsp (reverse path) "/") "\n")
                            st)))
              (next (=> st ((dsp-node cursor path) st))))))))
  (dsp-node inventory '()))

(define (dsp-bundle bundle)
  (fmt-join dsp-package (bundle-packages bundle) "\n"))

(define (dsp-db-item-identifier item)
  (dsp-package-identifier (database-item-package item)))

(define (dsp-package-identifier package)
  (cat (dsp-package-name package)
       " " (dsp-package-version (package-version package))))

(define (dsp-package-name package)
  (dsp (package-name package)))

(define (dsp-db-item/short item)
  (cat (if (database-item-installed? item) "i" "u")
       " " (dsp-db-item-identifier item)))

(define (dsp-package-version version)
  (cond ((null? version)
         (dsp "()"))
        (else
         (fmt-join (lambda (part)
                     (fmt-join dsp part "."))
                   version
                   "-"))))
)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
