;;; cmdline.sls --- Command-line UI library

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
#!r6rs

(library (dorodango ui cmdline)
  (export run-cmdline-ui)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) drop concatenate unfold)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13)
                string-null?
                string-prefix?
                string-suffix?
                string-tokenize
                string-trim-both)
          (srfi :14 char-sets)
          (srfi :39 parameters)
          (srfi :67 compare-procedures)
          (spells define-values)
          (spells alist)
          (only (spells misc) and=> unspecific)
          (spells fmt)
          (spells match)
          (spells foof-loop)
          (spells nested-foof-loop)
          (spells xvector)
          (spells finite-types)
          (spells operations)
          (spells pathname)
          (spells filesys)
          (spells args-fold)
          (spells logging)
          (spells tracing)
          (only (spells record-types) define-record-type*)
          (dorodango private utils)
          (dorodango inventory)
          (dorodango package)
          (dorodango bundle)
          (dorodango repository)
          (dorodango destination)
          (dorodango database)
          (dorodango database dependencies)
          (dorodango solver)
          (dorodango solver choice)
          (prefix (dorodango solver universe)
                  universe-)
          (dorodango config)
          (dorodango actions)
          (dorodango ui formatters)
          (dorodango ui))


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


;;; UI helpers

(define (fmt/stdout . formats)
  (fmt #t (apply-cat formats))
  (flush-output-port (current-output-port)))

(define (cmdline/message . formats)
  (fmt/stdout (cat (apply-cat formats) nl)))

(define (cmdline/y-or-n default message)
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

(define (cmdline/prompt message choices)
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

(define (make-cmdline-ui)
  (object #f
    ((ui/message ui . formats)
     (apply cmdline/message formats))
    ((ui/y-or-n ui default message)
     (cmdline/y-or-n default message))
    ((ui/prompt ui message choices)
     (cmdline/prompt message choices))))


;;; Command-line processing

(define (make-help-option command)
  (option
   '("help" #\h) #f #f
   (lambda (option option-name arg vals)
     (acons 'run
            (lambda (vals)
              (fmt #t (dsp-help command))
              '())
            vals))))

(define (dsp-option-name name)
  (cat (if (string? name) "--" "-") name))

(define (dsp-option/left-side option)
  (cat (fmt-join dsp-option-name (option-names option) ", ")
       (cond ((option-argument option)
              => (lambda (metavar)
                   (cat " " (string-upcase (symbol->string metavar)))))
             (else
              ""))))

(define (dsp-help command)
  (let ((synopsis (command-synopsis command))
        (description (command-description command)))
    (cat "Usage: doro " (car synopsis) "\n"
         (fmt-join/suffix dsp (cdr synopsis) "\n")
         (fmt-indented "  " (car description))
         (fmt-join dsp (cdr description) "\n")
         "\n"
         "Options:\n"
         (dsp-listing "  " (append
                            (map (lambda (option)
                                   (dsp-option/left-side option))
                                 (command-options command))
                            '("--help"))
                      "  " (append (map option-description (command-options command))
                                   '("show this help and exit")))
         "\n"
         (apply-cat (command-footer command)))))

(define (dsp-version)
  (cat "doro 0.0.0\n"
       "Copyright (C) Andreas Rottmann\n"
       (wrap-lines
        "This is free software; see the source for copying conditions."
        "There is NO warranty; not even for MERCHANTABILITY or FITNESS "
        "FOR A PARTICULAR PURPOSE.")
       "\n"))

;; This could use a better name
(define (dsp-listing indent left-items separator right-items)
  (lambda (st)
    (let* ((left-sides
            (map (lambda (left)
                   (fmt #f (cat indent left)))
                 left-items))
           (left-width (fold-left max 0 (map string-length left-sides))))
      ((apply-cat
        (map (lambda (left right)
               (columnar left-width (dsp left)
                         separator
                         (with-width (- 78 left-width) (wrap-lines right))))
             left-sides right-items))
       st))))


;;; Commands

(define %commands '())

(define (command-list)
  (reverse %commands))

(define-record-type* command
  (make-command name description synopsis footer options handler)
  ())

(define (clause-alist->command name clauses)
  (define (list-clause name)
    (cond ((assq name clauses) => cdr)
          (else                   '())))
  (make-command name
                (list-clause 'description)
                (list-clause 'synopsis)
                (list-clause 'footer)
                (list-clause 'options)
                (cond ((assq 'handler clauses)
                       => cadr)
                      (else
                       (assertion-violation 'clause-alist->command
                                            "handler clause missing")))))

(define (find-command name)
  (find (lambda (command)
          (eq? name (command-name command)))
        %commands))

(define-syntax define-command
  (syntax-rules (description synopsis options handler)
    ((_ name (clause-name clause-content ...) ...)
     (define-values ()
       (set! %commands (cons (clause-alist->command
                              'name
                              (list (list 'clause-name clause-content ...)
                                    ...))
                             %commands))))))

(define (arg-pusher name)
  (lambda (option option-name arg vals)
    (apush name arg vals)))

(define arg-setter
  (case-lambda
    ((name convert)
     (lambda (option option-name arg vals)
       (acons name (convert arg) vals)))
    ((name)
     (arg-setter name values))))

(define (value-setter name value)
  (lambda (option option-name arg vals)
    (acons name value vals)))

(define-syntax define-option
  (syntax-rules ()
    ((_ identifier names argument description processor)
     (define identifier
       (option 'names
               'argument
               #f
               #f
               description
               processor)))))

(define-option bundle-option ("bundle" #\b) bundle
  "additionally consider packages from BUNDLE"
  (arg-pusher 'bundles))

(define-option no-depends-option ("no-depends") #f
  "ignore dependencies"
  (value-setter 'no-depends? #t))

(define-option force-option ("force") #f
  "force operation"
  (value-setter 'force? #t))

(define (parse-package-string s)
  (cond ((maybe-string->package s "=")
         => (lambda (package)
              (values (package-name package)
                      (package-version package))))
        (else
         (values (string->symbol s) #f))))

(define (find-db-items db packages)
  (loop ((for package (in-list packages))
         (for result
              (listing
               (receive (name version) (parse-package-string package)
                 (database-lookup db name version)))))
    => (reverse result)))


;;; Querying

(define-command list
  (synopsis "list")
  (description "List packages.")
  (options (option '("all") #f #f #f
                   "also show available packages"
                   (value-setter 'all? #t))
           bundle-option)
  (handler
   (lambda (vals)
     (let ((all? (assq-ref vals 'all?))
           (db (config->database vals)))
       (database-add-bundles! db (opt-ref/list vals 'bundles))
       (loop ((for package items (in-database db (sorted-by symbol<?))))
         (cond (all?
                (fmt #t (fmt-join/suffix dsp-db-item/short items "\n")))
               ((find database-item-installed? items)
                => (lambda (installed)
                     (fmt #t (dsp-db-item/short installed) "\n")))))))))

(define-command show
  (description "Show package information.")
  (options bundle-option)
  (synopsis "show [--bundle BUNDLE]... PACKAGE...")
  (handler
   (lambda (vals)
     (let ((packages (opt-ref/list vals 'operands))
           (db (config->database vals)))
       (database-add-bundles! db (opt-ref/list vals 'bundles))
       (loop ((for item (in-list (find-db-items db packages))))
         (fmt #t (dsp-db-item item)))))))

(define-command show-bundle
  (synopsis "show-bundle BUNDLE...")
  (description "Show bundle contents.")
  (handler
   (lambda (vals)
     (loop ((for bundle-location (in-list (opt-ref/list vals 'operands))))
       (let ((bundle (open-input-bundle bundle-location)))
         (fmt #t (dsp-bundle bundle)))))))

(define (dsp-db-item item)
  (dsp-package (database-item-package item)))

(define (dsp-db-item/short item)
  (lambda (st)
    (let ((package  (database-item-package item))
          (width (fmt-width st)))
      ((cat (if (database-item-installed? item) "i" "u")
            " " (pad (min 32 (div width 3)) (package-name package))
            " " (dsp-package-version (package-version package)))
       st))))


;;; Package installation and removal

(define-command update
  (synopsis "update")
  (description "Update repository information")
  (handler
   (lambda (vals)
     (let ((db (config->database vals)))
       (database-update! db)
       (close-database db)))))

(define (select-package/string db package-string)
  (receive (name version) (parse-package-string package-string)
    (select-package db name version)))

(define (select-package db name version)
  (let ((item (database-lookup db name version)))
    (cond ((not item)
           (die (cat "could not find any package matching `"
                     name (if (package-version? version)
                              (cat "-" (dsp-package-version version))
                              fmt-null)
                     "'")))
          (else
           (database-item-package item)))))

(define (install-command vals)
  (let ((bundle-locations (opt-ref/list vals 'bundles))
        (packages (opt-ref/list vals 'operands))
        (no-depends? (assq-ref vals 'no-depends?))
        (db (config->database vals)))
    (database-add-bundles! db bundle-locations)
    (loop ((for package (in-list packages))
           (for to-install (listing (select-package/string db package))))
      => (cond (no-depends?
                (loop ((for package (in-list to-install)))
                  (database-install! db package)))
               (else
                (apply-actions db to-install '()))))))

(define-command install
  (synopsis "install [--bundle BUNDLE]... PACKAGE...")
  (description "Install new packages.")
  (options bundle-option no-depends-option)
  (handler install-command))

(define (remove-command vals)
  (let ((packages (opt-ref/list vals 'operands))
        (no-depends? (assq-ref vals 'no-depends?))
        (db (config->database vals)))
    (cond (no-depends?
           (loop ((for package-name (in-list packages)))
             (unless (database-remove! db (string->symbol package-name))
               (message "Package " package-name " was not installed."))))
          (else
           (loop ((for package-name (in-list packages))
                  (for to-remove (listing (string->symbol package-name))))
             => (apply-actions db '() to-remove))))))

(define-command remove
  (description "Remove packages.")
  (synopsis "remove PACKAGE...")
  (options no-depends-option)
  (handler remove-command))

(define (upgrade-command vals)
  (let ((packages (opt-ref/list vals 'operands))
        (db (config->database vals)))
    (define (select-upgrade package-name)
      (and-let* ((installed (database-lookup db package-name 'installed))
                 (item (database-lookup db package-name 'newest)))
        (database-item-package item)))
    (loop ((for package-name (in-list (if (null? packages)
                                          (database-package-names db)
                                          (map string->symbol packages))))
           (for to-upgrade (listing (select-upgrade package-name) => values)))
      => (apply-actions db to-upgrade '()))))

(define-command upgrade
  (description "Upgrade packages.")
  (synopsis "upgrade [PACKAGE...]")
  (options)
  (handler upgrade-command))


;;; Configuration

(define (config-command vals)
  (let* ((config (assq-ref vals 'config))
         (operands (opt-ref/list vals 'operands))
         (n-operands (length operands)))
    (if (null? operands)
        (fmt #t (dsp-config config))
        (case (string->symbol (car operands))
          ((destination)
           (unless (<= 3 n-operands 4)
             (die "`config destination' requires 2 or 3 arguments"))
           (let ((destination (config-item-destination
                               (config-default-item config)))
                 (package (string->package (list-ref operands 1) "="))
                 (category (string->symbol (list-ref operands 2)))
                 (pathname (if (> n-operands 3)
                               (->pathname (list-ref operands 3))
                               (make-pathname #f '() #f))))
             (for-each
              (lambda (pathname)
                (fmt #t (dsp-pathname pathname) "\n"))
              (destination-pathnames destination package category pathname))))))))

(define (dsp-config config)
  (define (dsp-config-item item)
    (let ((dest (config-item-destination item)))
      (cat (if (eq? (destination-name dest) (config-default-name config))
               "*"
               "-")
           " name: " (destination-name dest) "\n"
           "  database: "
           (->namestring (config-item-database-location item)) "\n"
           "  cache-directory: "
           (->namestring (config-item-cache-directory item)) "\n"
           "  repositories:\n"
           (fmt-indented "    " (fmt-join dsp-repository
                                          (config-item-repositories item))))))
  (define (dsp-repository repo)
    (cat "- " (repository-name repo) ": " (repository-location repo) "\n"))
  (cat "default-implementation: " (config-default-implementation config) "\n"
       "destinations:\n"
       (fmt-indented "  " (fmt-join dsp-config-item (config-items config)))))

(define-command config
  (description "Show configuration.")
  (synopsis "config destination PACKAGE CATEGORY [FILENAME]")
  (options)
  (handler config-command))

(define (init-command vals)
  (let* ((config (assq-ref vals 'config))
         (operands (opt-ref/list vals 'operands))
         (n-operands (length operands))
         (destination
          (case n-operands
            ((0)  #f)
            ((1)  (string->symbol (car operands)))
            (else (die "`setup-destination' takes zero or one arguments")))))
    (config->database (append (if destination
                                  '((destination . ,destination))
                                  '())
                              vals))))

(define-option implementation-option ("implementation" #\i) implementation
  "use IMPLEMENTATION in destination"
  (arg-setter 'implementation string->symbol))

(define-command init
  (description "Initialize a destination.")
  (synopsis "init [OPTIONS] [DESTINATION]")
  (options implementation-option)
  (handler init-command))


;;; Packaging

(define (create-bundle-command vals)
  (define (read-packages-list pkg-list-files append-version)
    (collect-list (for pathname (in-list pkg-list-files))
      (let ((packages (call-with-input-file (->namestring pathname)
                        read-pkg-list)))
        (if (null? append-version)
            packages
            (map (lambda (package)
                   (package-modify-version
                    package
                    (lambda (version)
                      (append version append-version))))
                 packages)))))
  (define (compute-bundle-name packages)
    (match packages
      (()
       (die "all package lists have been empty."))
      ((package)
       (package->string package "_"))
      (_
       (die "multiple packages found and no bundle name specified."))))
  (let ((directories (match (opt-ref/list vals 'operands)
                       (()
                        (list (make-pathname #f '() #f)))
                       (operands
                        (map pathname-as-directory operands))))
        (output-directory (or (and=> (assq-ref vals 'output-directory)
                                     pathname-as-directory)
                              (make-pathname #f '() #f)))
        (output-filename (assq-ref vals 'output-filename))
        (append-version (or (and=> (assq-ref vals 'append-version)
                                   string->package-version)
                            '())))
    (let ((pkg-list-files (find-pkg-list-files directories))
          (need-rewrite? (not (null? append-version))))
      (when (null? pkg-list-files)
        (die (cat "no package lists found in or below "
                  (fmt-join dsp-pathname pkg-list-files ", ")) "."))
      (let* ((packages-list (read-packages-list pkg-list-files append-version))
             (output
              (or output-filename
                  (->namestring
                   (pathname-with-file
                    output-directory
                    (compute-bundle-name (concatenate packages-list)))))))
        (create-bundle output
                       (map (lambda (pathname)
                              (pathname-with-file pathname #f))
                            pkg-list-files)
                       packages-list
                       need-rewrite?)))))

(define (read-pkg-list port)
  (unfold eof-object?
          parse-package-form
          (lambda (seed) (read port))
          (read port)))

(define-command create-bundle
  (description "Create a bundle.")
  (synopsis "create-bundle [DIRECTORY...]")
  (options (option '("output" #\o) 'filename #f #f
                   "bundle filename"
                   (arg-setter 'output-filename))
           (option '("directory" #\d) 'directory #f #f
                   "output directory when using implicit filename"
                   (arg-setter 'output-directory))
           (option '("append-version") 'version #f #f
                   "append VERSION to each package's version"
                   (arg-setter 'append-version)))
  (handler create-bundle-command))

(define (scan-bundles-command vals)
  (define (do-scan port)
    (iterate! (for directory (in-list (opt-ref/list vals 'operands)))
        (for entry (in-list (scan-bundles-in-directory directory directory)))
      (match entry
        ((package . bundle-pathname)
         (fmt port
              (pretty/unshared
               (package->form (package-with-property
                               package
                               'location
                               (list (pathname->location bundle-pathname))))))))))
  (let-assq vals (output-filename)
    (if output-filename
        (call-with-output-file/atomic output-filename do-scan)
        (do-scan (current-output-port)))
    (unspecific)))

(define-command scan-bundles
  (description "Scan one or more directories for bundles.")
  (synopsis "scan-bundles DIRECTORY...")
  (options (option '("output" #\o) 'filename #f #f
                   "output scan results to FILENAME"
                   (arg-setter 'output-filename)))
  (handler scan-bundles-command))


(define (symlink-command vals)
  (define (string->package-list string)
    (map string->symbol (string-tokenize
                         string
                         (char-set-complement (string->char-set " ,")))))
  (let ((force? (assq-ref vals 'force?))
        (deep? (assq-ref vals 'deep?))
        (include (and=> (assq-ref vals 'include) string->package-list))
        (exclude (and=> (assq-ref vals 'exclude) string->package-list)))
    (match (opt-ref/list vals 'operands)
      ((bundle-directory target-directory)
       (symlink-bundle bundle-directory
                       target-directory
                       force?
                       deep?
                       (lambda (package)
                         (cond ((and include exclude)
                                (and (memq (package-name package) include)
                                     (not (memq (package-name package) exclude))))
                               (include
                                (memq (package-name package) include))
                               (exclude
                                (not (memq (package-name package) exclude)))
                               (else
                                #t)))))
      (_
       (die "`symlink' expects two arguments")))))

(define-command symlink-bundle
  (description "Create symbolink links for a bundle.")
  (synopsis "symlink-bundle BUNDLE-DIRECTORY TARGET-DIRECTORY")
  (options force-option
           (option '("deep") #f #f #f
                   "symlink only files"
                   (value-setter 'deep? #t))
           (option '("include") 'packages #f #f
                   "only consider PACKAGES (space or comma separated list)"
                   (arg-setter 'include))
           (option '("exclude") 'packages #f #f
                   "don't consider PACKAGES (space or comma separated list)"
                   (arg-setter 'exclude)))
  (handler symlink-command))


;;; Entry point

(define (process-command-line command cmd-line seed-vals)
  (define (unrecognized-option option name arg vals)
    (die (cat "unrecognized option: " name)))
  (define (process-operand operand vals)
    (apush 'operands operand vals))
  (let ((vals (args-fold* cmd-line
                          (cons (make-help-option command)
                                (command-options command))
                          #t
                          unrecognized-option
                          process-operand
                          seed-vals)))
    (cond (((or (assq-ref vals 'run)
                (command-handler command))
            vals)
           (exit))
          (else
           (fmt #t "Aborted.\n")
           (exit #f)))))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "dorodango") "config.scm")))

(define (config->database options)
  (let* ((config (assq-ref options 'config))
         (destination (or (assq-ref options 'destination)
                          (config-default-name config)))
         (implementation (or (assq-ref options 'implementation)
                             (config-default-implementation config)))
         (item (if destination
                   (or (config-ref config destination)
                       (die (cat "no such destination configured: " destination)))
                   (config-default-item config))))
    (open-database (config-item-database-location item)
                   (config-item-destination item)
                   (config-item-repositories item)
                   implementation
                   (config-item-cache-directory item))))

(define-option config-option ("config" #\c) file
  (cat "use configuration in FILE"
       " (default: `" (dsp-pathname (default-config-location)) "')")
  (arg-setter 'config))

(define-option no-config-option ("no-config") #f
  "do not read a configuration file"
  (value-setter 'config #f))

(define-option prefix-option ("prefix") prefix
  "set installation prefix and database location"
  (arg-setter 'prefix))

(define-option destination-option ("dest" #\d) destination
  "select configured destination"
  (arg-setter 'destination string->symbol))

(define-option version-option ("version" #\V) #f
  "show version information and exit"
  (lambda (option name arg vals)
    (acons 'run (lambda (vals) (fmt #t (dsp-version)) '()) vals)))

(define (main-handler vals)
  (define (read-config/default pathname)
    (guard (c ((i/o-file-does-not-exist-error? c)
               (cond (pathname
                      (die (cat "specified config file `"
                                (dsp-pathname pathname) "' does not exist.")))
                     (else (default-config)))))
      (call-with-input-file (->namestring pathname)
        read-config)))
  (define (config-with-prefix config prefix)
    (if prefix
        (make-prefix-config prefix
                            (config-item-repositories
                             (config-default-item config))
                            (config-default-implementation config))
        config))
  (let ((operands (opt-ref/list vals 'operands))
        (prefix (assq-ref vals 'prefix)))
    (cond ((null? operands)
           (fmt #t (dsp-help (find-command 'main))))
          ((find-command (string->symbol (car operands)))
           => (lambda (command)
                (let ((config (cond ((assq-ref vals 'config)
                                     => read-config/default)
                                    (else
                                     (default-config)))))
                  (process-command-line
                   command
                   (cdr operands)
                   `((operands . ())
                     (config . ,(config-with-prefix config prefix)))))))
          (else
           (error 'main "unknown command" (car operands))))))

(define-command main
  (synopsis "[OPTIONS] COMMAND [COMMAND-OPTIONS] [ARGS]\n")
  (description
   (wrap-lines
    "doro is a command-line interface for downloading, installing "
    "and inspecting packages containing R6RS libraries and programs.")
   ""
   "Commands:"
   ""
   (dsp-listing "  " (map command-name (command-list))
                "  " (map (lambda (command)
                            (apply-cat (command-description command)))
                          (command-list))))
  (footer "Use \"doro COMMAND --help\" to get more information about COMMAND.\n"
          (pad/both 72 "This doro has Super Ball Powers.")
          "\n")
  (options no-config-option config-option
           prefix-option
           destination-option
           version-option)
  (handler main-handler))

(define (make-message-log-handler name-drop)
  (define (titlecase s)
    (if (= 0 (string-length s))
        s
        (string-append (string (char-titlecase (string-ref s 0)))
                       (substring s 1 (string-length s)))))
  (lambda (entry)
    (let ((obj (log-entry-object entry))
          (level-name (log-entry-level-name entry))
          (name (drop (log-entry-logger-name entry) name-drop))
          (default-level? (eq? (log-entry-level-name entry) 'info)))
      (let ((prefix (cat (if default-level?
                             fmt-null
                             (cat "doro: " level-name ": "))
                         (if (or default-level? (null? name))
                             fmt-null
                             (cat "[" (fmt-join dsp name ".") "] "))))
            (output (call-with-string-output-port
                      (lambda (port)
                        (if (procedure? obj)
                            (obj port)
                            (display obj port))))))
        (message prefix (if default-level?
                            (titlecase output)
                            output))))))

(define (run-cmdline-ui argv)
  (for-each
   (match-lambda
    ((logger . properties)
     (set-logger-properties!
      logger
      properties)))
   `((,logger:dorodango
      (handlers (info ,(make-message-log-handler 1))))
     (,logger:dorodango.db
      (propagate? #f)
      (handlers (info ,(make-message-log-handler 2))))
     (,logger:dorodango.solver
      (propagate? #f)
      (handlers (warning ,(make-message-log-handler 1))))))
  (parameterize ((current-ui (make-cmdline-ui)))
    (process-command-line (find-command 'main)
                          (cdr argv)
                          `((operands)
                            (config . ,(default-config-location))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (object 1))
;; End:
