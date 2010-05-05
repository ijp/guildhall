;;; dependencies.sls --- dependency resolution UI

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (dorodango ui cmdline dependencies)
  (export apply-actions)
  (import (rnrs)
          (srfi :8 receive)
          (srfi :67 compare-procedures)
          (wak fmt)
          (wak foof-loop)
          (wak foof-loop nested)
          (spells finite-types)
          (spells record-types)
          (spells xvector)
          (spells match)
          (spells tracing) ;debug
          (only (spells misc) topological-sort)
          (dorodango private utils)
          (dorodango package)
          (dorodango solver)
          (dorodango solver choice)
          (prefix (dorodango solver universe)
                  universe-)
          (dorodango hooks)
          (dorodango database)
          (dorodango database dependencies)
          (dorodango ui formatters)
          (dorodango ui))


;;; Dependency management

(define (apply-actions db to-install to-remove)
  (receive (universe package-table) (database->universe db)
    (let ((irreparable (irreparable-packages universe)))
      (cond ((null? irreparable)
             (run-setup db (database-unpacked-versions db package-table))
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
                          (fmt-indented
                           "  "
                           (wrap-lines
                            (fmt-join (lambda (package)
                                        (dsp (package-name package)))
                                      irreparable
                                      " ")))
                          "Remove these packages and proceed?"))
             (for-each (lambda (package)
                         (database-remove! db (package-name package)))
                       irreparable)
             (apply-actions db to-install to-remove))
            (else
             #f)))))

(define (apply-choices db choices)
  (loop continue ((for choice (in-choice-set choices))
                  (with unpacked-versions '()))
    => (run-setup db unpacked-versions)
    (let ((version (choice-version choice)))
      (cond ((universe-version-tag version)
             (if (database-unpack! db (universe-version->package version))
                 (continue
                  (=> unpacked-versions (cons version unpacked-versions)))
                 (continue)))
            (else
             (database-remove! db (universe-version-package-name version))
             (continue))))))

(define (database-unpacked-versions db package-table)
  (loop continue ((for package-name items (in-database db))
                  (with result '()))
    => (reverse result)
    (cond ((find (lambda (item) (eq? 'unpacked (database-item-state item)))
                 items)
           (let ((version (universe-package-current-version
                           (hashtable-ref package-table package-name #f))))
             (continue (=> result (cons version result)))))
          (else
           (continue)))))

(define (calculate-setup-sequence versions)
  (reverse (topological-sort (versions->setup-graph versions) eq?)))

(define (run-setup db versions)
  (loop ((for package-name (in-list (calculate-setup-sequence versions))))
    (guard (c ((hook-runner-exception? c)
               (fmt (current-error-port)
                    (dsp-hook-runner-exception c))))
      (database-setup! db package-name))))

(define (versions->setup-graph versions)
  (let ((version-table (make-hashtable universe-version-hash universe-version=?)))
    (iterate! (for version (in-list versions))
      (hashtable-set! version-table version #t))
    (collect-list (for version (in-vector (hashtable-keys version-table)))
      (cons (universe-version-package-name version)
            (collect-list
                (for dependency (in-list (universe-version-dependencies version)))
                (for target (in-list (universe-dependency-targets dependency)))
                (if (hashtable-contains? version-table target))
              (universe-version-package-name target))))))

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
        (append '((#\y "accept this solution"))
                (if exhausted? '() '((#\n "Try to find another solution")))
                '((#\q "quit the operation"))
                (if (< index (- (xvector-length solutions) 1))
                    '((#\. "select next solution"))
                    '())
                (if (> index 0)
                    '((#\, "select previous solution"))
                    '())
                `((#\o
                   ,(cat "toggle between the contents of the solution and"
                         "an explanation of the solution")))))
      (loop continue ((with selected-index #f)
                      (with show-story? #f)
                      (with exhausted? #f))
        (define (solution-prompt now-exhausted? index)
          (define (iterate new-index)
            (continue (=> selected-index new-index)
                      (=> exhausted? (or exhausted? now-exhausted?))))
          (let*-values (((solution) (xvector-ref solutions index))
                        ((risky? assess-message)
                         (assess-solution solution
                                          initial-choices
                                          package-table))
                        ((solution-message)
                         (if show-story?
                             (dsp-solution solution)
                             assess-message)))
            (cond
              (risky?
               (case (prompt
                      (if now-exhausted?
                          (cat "No more solutions. Proceed with previous solution?")
                          (cat solution-message
                               "Accept this solution?"))
                      (prompt-choices index (or exhausted? now-exhausted?)))
                 ((#\y)       (solution->actions (xvector-ref solutions index)))
                 ((#\n)       (iterate #f))
                 ((#\q)       #f)
                 ((#\.)       (iterate (+ index 1)))
                 ((#\,)       (iterate (- index 1)))
                 ((#\o)
                  (continue (=> selected-index index)
                            (=> show-story? (not show-story?))))
                 (else
                  (assert #f))))
              ((y-or-n #t (cat solution-message "Do you want to continue?"))
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

;; Calculate details on the action implied by changing (or not)
;; `current-version' to `version', where the version chosen by the
;; user is `chosen-version'.
(define (compute-action current-version chosen-version version)
  (let ((to-install (and chosen-version
                         (universe-version-tag chosen-version)))
        (to-remove? (and chosen-version
                         (not (universe-version-tag chosen-version)))))
    (cond ((and (not current-version) version)
           (make-package-action
            (action-type install)
            (cond (to-install  (action-compliance ordered))
                  (to-remove?  (action-compliance disobedient))
                  (else        (action-compliance auto)))))
          ((and current-version (not version))
           (make-package-action
            (action-type remove)
            (cond (to-remove?  (action-compliance ordered))
                  (to-install  (action-compliance disobedient))
                  (else        (action-compliance auto)))))
          ((and current-version version)
           (let ((type (if3 (package-version-compare current-version version)
                            (action-type upgrade)
                            (action-type keep)
                            (action-type downgrade))))
             (make-package-action
              type
              (cond (to-remove?
                     (action-compliance disobedient))
                    (to-install
                     (if3 (package-version-compare current-version to-install)
                          (if (eq? (action-type upgrade) type)
                              (action-compliance ordered)
                              (action-compliance disobedient))
                          (action-compliance already)
                          (if (eq? (action-type downgrade) type)
                              (action-compliance ordered)
                              (action-compliance disobedient))))
                    (else
                     (action-compliance auto))))))
          (else
           (make-package-action (action-type keep)
                                (if chosen-version
                                    (action-compliance disobedient)
                                    (action-compliance ordered)))))))

;; Returns two values: wether a solution is "risky" (when any action
;; taken has compliance `disobedient') 
(define (assess-solution solution initial-choices package-table)
  (define (package-name.action<? p1 p2)
    (symbol<? (car p1) (car p2)))
  (let ((action-lists (make-vector (vector-length action-types) '())))
    (define (accumulate-message)
      (loop ((for type (in-vector action-types))
             (for action-heading (in-vector action-type-headings))
             (let action-list (vector-ref action-lists (action-type-index type)))
             (for message-parts
                  (listing (cat action-heading "\n"
                                (fmt-indented "  " (dsp-action-list action-list)))
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
                                     chosen-version
                                     (universe-version-tag (choice-version choice))))
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

;; Calculate the options for the solver, based on the the `to-install'
;; (list of `package') and `to-remove' (list of package names).
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

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop as-match)
;; End:
