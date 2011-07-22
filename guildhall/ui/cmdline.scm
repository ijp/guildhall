;;; cmdline.scm --- Command-line UI library

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

(library (guildhall ui cmdline)
  (export command-list
          run-cmdline-ui)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) drop concatenate)
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
          (guildhall ext fmt)
          (guildhall ext foof-loop)
          (guildhall ext foof-loop nested)
          (guildhall ext define-values)
          (only (guile) assq-ref acons variable-ref)
          (only (spells misc) and=> unspecific)
          (ice-9 match)
          (spells operations)
          (spells pathname)
          (spells filesys)
          (spells args-fold)
          (spells logging)
          (spells tracing)
          (only (spells record-types) define-record-type*)
          (guildhall private utils)
          (guildhall inventory)
          (guildhall package)
          (guildhall bundle)
          (guildhall repository)
          (guildhall destination)
          (guildhall hooks)
          (guildhall database)
          (guildhall config)
          (guildhall actions)
          (only (guildhall solver) logger:dorodango.solver)
          (guildhall ui)
          (guildhall ui formatters)
          (guildhall ui cmdline base)
          (guildhall ui cmdline help)
          (guildhall ui cmdline dependencies))


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
              (appending-reverse
               (receive (name version) (parse-package-string package)
                 (if version
                     (or (and=> (database-lookup db name version) list)
                         '())
                     (database-items db name))))))
    => (reverse result)))


;;; Querying

(define-command list
  (synopsis "list")
  (description "List packages.")
  (options (option '("all" #\a) #f #f #f
                   "also show available packages"
                   (value-setter 'all? #t))
           bundle-option)
  (handler
   (lambda (vals)
     (let ((all? (assq-ref vals 'all?)))
       (call-with-database* vals
         (lambda (db)
           (database-add-bundles! db (opt-ref/list vals 'bundles))
           (loop ((for package items (in-database db (sorted-by symbol<?))))
             (cond (all?
                    (fmt #t (fmt-join/suffix dsp-db-item/short items "\n")))
                   ((find database-item-installed? items)
                    => (lambda (installed)
                         (fmt #t (dsp-db-item/short installed) "\n")))))))))))

(define-command show
  (description "Show package information.")
  (options bundle-option)
  (synopsis "show [--bundle BUNDLE]... PACKAGE...")
  (handler
   (lambda (vals)
     (let ((packages (opt-ref/list vals 'operands)))
       (call-with-database* vals
         (lambda (db)
           (database-add-bundles! db (opt-ref/list vals 'bundles))
           (fmt #t (fmt-join dsp-db-item
                             (find-db-items db packages)
                             "\n"))))))))

(define-command show-bundle
  (synopsis "show-bundle BUNDLE...")
  (description "Show bundle contents.")
  (handler
   (lambda (vals)
     (loop ((for bundle-location (in-list (opt-ref/list vals 'operands))))
       (let ((bundle (open-input-bundle bundle-location)))
         (fmt #t (dsp-bundle bundle)))))))

(define-command man
  (synopsis "man")
  (description "Show help for all commands in groff (man page) format")
  (handler
   (lambda (vals)
     (fmt #t (dsp-man-page (command-list))))))

(define (dsp-db-item item)
  (dsp-package (database-item-package item)
               (cat "Status: " (database-item-state item) "\n")))

(define (dsp-db-item/short item)
  (lambda (st)
    (let ((package  (database-item-package item))
          (width (fmt-width st)))
      ((cat (case (database-item-state item)
              ((installed) "i")
              ((unpacked)  "U")
              (else        "n"))
            " " (pad (min 32 (div width 3)) (package-name package))
            " " (pad (min 20 (div width 4))
                     (dsp-package-version (package-version package)))
            " " (package-synopsis package))
       st))))


;;; Package installation and removal

(define-command update
  (synopsis "update")
  (description "Update repository information")
  (handler
   (lambda (vals)
     (call-with-database* vals
       (lambda (db)
         (database-update! db))))))

(define (select-package/string db package-string)
  (receive (name version) (parse-package-string package-string)
    (select-package db name (or version 'newest))))

(define (select-package db name version)
  (let ((item (database-lookup db name version)))
    (cond ((not item)
           (fatal (cat "could not find any package matching `"
                       name (if (package-version? version)
                                (cat "-" (dsp-package-version version))
                                fmt-null)
                       "'")))
          (else
           (database-item-package item)))))

(define (install-command vals)
  (let ((bundle-locations (opt-ref/list vals 'bundles))
        (packages (opt-ref/list vals 'operands))
        (no-depends? (assq-ref vals 'no-depends?)))
    (call-with-database* vals
      (lambda (db)
        (database-add-bundles! db bundle-locations)
        (loop ((for package (in-list packages))
               (for to-install (listing (select-package/string db package))))
          => (cond (no-depends?
                    (install/no-depends db to-install))
                   (else
                    (apply-actions db to-install '()))))))))

(define (install/no-depends db to-install)
  (loop ((for package (in-list to-install)))
    (cond ((database-unpack! db package)
           (guard (c ((hook-runner-exception? c)
                      (fmt (current-error-port) (dsp-hook-runner-exception c))))
             (database-setup! db (package-name package))))
          (else
           (let ((db-package
                  (database-lookup db (package-name package) 'installed)))
             (message "Package " package-name
                      " already at version " (package-version db-package)))))))

(define-command install
  (synopsis "install [--bundle BUNDLE]... PACKAGE...")
  (description "Install new packages.")
  (options bundle-option no-depends-option)
  (handler install-command))

(define (remove-command vals)
  (define (not-installed-error package-name)
    (fatal (cat "package `" package-name "' is not installed.")))
  (let ((packages (opt-ref/list vals 'operands))
        (no-depends? (assq-ref vals 'no-depends?)))
    (call-with-database* vals
      (lambda (db)
        (cond (no-depends?
               (loop ((for package-name (in-list packages)))
                 (unless (database-remove! db (string->symbol package-name))
                   (not-installed-error package-name))))
              (else
               (loop ((for package-string (in-list packages))
                      (let-values (package-name is-installed?)
                        (let ((name (string->symbol package-string)))
                          (values name
                                  (and (database-lookup db name 'installed) #t))))
                      (for to-remove (listing package-name
                                              (if is-installed?))))
                 => (apply-actions db '() to-remove)
                 (unless is-installed?
                   (not-installed-error package-string)))))))))

(define-command remove
  (description "Remove packages.")
  (synopsis "remove PACKAGE...")
  (options no-depends-option)
  (handler remove-command))

(define (upgrade-command vals)
  (define (select-upgrade items)
    (and-let* ((item (car items))
               ((exists database-item-installed? items))
               ((not (database-item-installed? item))))
      (database-item-package item)))
  (call-with-database* vals
    (lambda (db)
      (loop ((for package-name items (in-database db))
             (for to-upgrade (listing (select-upgrade items) => values)))
        => (apply-actions db to-upgrade '())))))

(define-command upgrade
  (description "Upgrade all packages.")
  (synopsis "upgrade")
  (options)
  (handler upgrade-command))

(define-command clean
  (description "Clean the package cache.")
  (synopsis "clean")
  (handler
   (lambda (vals)
     (call-with-database* vals
       (lambda (db)
         (database-clear-cache! db))))))


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
             (fatal "`config destination' requires 2 or 3 arguments"))
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
  (cat "destinations:\n"
       (fmt-indented "  " (fmt-join dsp-config-item (config-items config)))))

(define-command config
  (description "Show configuration.")
  (synopsis "config"
            "config destination PACKAGE CATEGORY [FILENAME]")
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
            (else (fatal "`setup-destination' takes zero or one arguments")))))
    (call-with-database* (append (if destination
                                     '((destination . ,destination))
                                     '())
                                 vals)
      (lambda (db)
        ;; no need to do anything
        (unspecific)))))

(define-command init
  (description "Initialize a destination.")
  (synopsis "init [OPTIONS] [DESTINATION]")
  (handler init-command))


;;; Packaging

(define (create-bundle-command vals)
  (define (compute-bundle-filename packages)
    (match packages
      (()
       (fatal "all package lists have been empty."))
      ((package)
       (package->string package "_"))
      (_
       (fatal "multiple packages found and no bundle name specified."))))
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
        (fatal (cat "no package lists found in or below "
                    (fmt-join dsp-pathname pkg-list-files ", ") ".")))
      (let* ((packages-list (read-package-lists pkg-list-files append-version))
             (output
              (or output-filename
                  (->namestring
                   (pathname-with-file
                    output-directory
                    (compute-bundle-filename (concatenate packages-list)))))))
        (create-bundle output
                       (map (lambda (pathname)
                              (pathname-with-file pathname #f))
                            pkg-list-files)
                       packages-list
                       need-rewrite?)))))

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
  (let ((output-filename (assq-ref vals 'output-filename)))
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
       (fatal "`symlink' expects two arguments")))))

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


;;; Command-line processing

(define (command-list)
  (reverse (variable-ref %commands)))

(define (make-help-option command)
  (option
   '("help" #\h) #f #f
   (lambda (option option-name arg vals)
     (acons 'run
            (lambda (vals)
              (fmt #t (dsp-help indented-help-formatter command))
              '())
            vals))))

(define (process-command-line command cmd-line seed-vals)
  (define (unrecognized-option option name arg vals)
    (fatal (cat "unrecognized option: " name)))
  (define (process-operand operand vals)
    (apush 'operands operand vals))
  (let ((vals (args-fold* cmd-line
                          (cons (make-help-option command)
                                (command-options command))
                          #t
                          unrecognized-option
                          process-operand
                          seed-vals)))
    ((or (assq-ref vals 'run)
         (command-handler command))
     vals)))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "dorodango") "config.scm")))

(define (config->database options)
  (let* ((config (assq-ref options 'config))
         (destination (or (assq-ref options 'destination)
                          (config-default-name config)))
         (repos (opt-ref/list options 'repositories))
         (item (if destination
                   (or (config-ref config destination)
                       (fatal (cat "no such destination configured: " destination)))
                   (config-default-item config)))
         (location (config-item-database-location item)))
    (guard (c ((database-locked-error? c)
               (fatal (cat "database locked: " (dsp-pathname location)))))
      (open-database location
                     (config-item-destination item)
                     (append repos (config-item-repositories item))
                     (config-item-cache-directory item)))))

(define (call-with-database* options proc)
  (call-with-database (config->database options)
    (lambda (db)
      (guard (c ((error? c)
                 (close-database db)
                 (raise c)))
        (proc db)))))

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
  "select configured destination named DESTINATION"
  (arg-setter 'destination string->symbol))

(define-option version-option ("version" #\V) #f
  "show version information and exit"
  (lambda (option name arg vals)
    (acons 'run (lambda (vals) (fmt #t (dsp-version)) '()) vals)))

(define-option repository-option ("repo" #\r) uri
  "add URI to the list of repositories to use"
  (lambda (option name arg vals)
    (apush 'repositories
           (or (uri-string->repository arg)
               (fatal (cat "unsupported repository URI: " arg)))
           vals)))

(define-option non-interactive-option ("non-interactive" #\n) #f
  "run non-interactively"
  (value-setter 'non-interactive? #t))

(define-option yes-option ("yes" #\y) #f
  "assume yes on prompts"
  (value-setter 'assume-yes? #t))

(define-option log-level-option ("log-level" #\l) level
  "set the log level (`error', `warning', `info', `debug' or `trace'; default is `info')"
  (arg-setter 'log-level string->symbol))

(define (main-handler vals)
  (define (read-config/guard pathname)
    (guard (c ((i/o-file-does-not-exist-error? c)
               (fatal (cat "specified config file `"
                           (dsp-pathname pathname) "' does not exist."))))
      (call-with-input-file (->namestring pathname)
        read-config)))
  (let ((operands (opt-ref/list vals 'operands))
        (prefix (assq-ref vals 'prefix))
        (log-level (or (assq-ref vals 'log-level) 'info)))
  (define (config-with-prefix config)
    (if prefix
        (make-prefix-config
         prefix
         (config-item-repositories (config-default-item config)))
        config))
    (parameterize ((current-ui (make-cmdline-ui
                                `((assume-yes? . ,(assq-ref vals 'assume-yes?))
                                  (non-interactive?
                                   . ,(assq-ref vals 'non-interactive?))))))
      (let-logger-properties
          ((logger:dorodango
            `((handlers (,log-level ,(make-message-log-handler 1)))))
           (logger:dorodango.db
            `((propagate? #f)
              (handlers (,log-level ,(make-message-log-handler 2)))))
           (logger:dorodango.solver
            `((propagate? #f)
              (handlers (warning ,(make-message-log-handler 1))))))
        (cond ((null? operands)
               (fmt #t (dsp-help indented-help-formatter
                                 (find-command 'main (variable-ref %commands)))))
              ((find-command (string->symbol (car operands))
                             (variable-ref %commands))
               => (lambda (command)
                    (let ((config (cond ((assq-ref vals 'config)
                                         => read-config/guard)
                                        (else
                                         (default-config)))))
                      (process-command-line
                       command
                       (cdr operands)
                       `((operands . ())
                         (repositories . ,(assq-ref vals 'repositories))
                         (destination . ,(assq-ref vals 'destination))
                         (config . ,(config-with-prefix config)))))))
              (else
               (fatal (cat "unknown command `" (car operands) "'"))))))))

(define-command main
  (synopsis "[OPTIONS] COMMAND [COMMAND-OPTIONS] [ARGS]\n")
  (description
   (wrap-lines
    "doro is a command-line interface for downloading, installing "
    "and inspecting packages containing R6RS libraries and programs.")
   ""
   "Commands:"
   ""
   (dsp-command-listing  (command-list)))
  (footer "Use \"doro COMMAND --help\" to get more information about COMMAND.\n"
          (pad/both 72 "This doro has Super Ball Powers.")
          "\n")
  (options no-config-option config-option
           prefix-option
           destination-option
           repository-option
           non-interactive-option
           yes-option
           version-option
           log-level-option)
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

(define (dsp-bundle-discrepancy discrepancy)
  (wrap-lines "This error is usually caused by stale bundles in the cache or "
              "repositories that provide bogus bundle information."))

(define (run-cmdline-ui argv)
  (guard (c ((fatal-error? c)
             (fmt (current-error-port) (cat "doro: " (condition-message c) "\n"))
             (exit #f))
            ((database-bundle-discrepancy? c)
             (fmt (current-error-port)
                  (cat "doro: discrepancy in bundle detected: "
                       (condition-message c) "\n"
                       (dsp-bundle-discrepancy c)))))
    (process-command-line (find-command 'main (variable-ref %commands))
                          (cdr argv)
                          `((operands)
                            (repositories . ())
                            (config . ,(default-config-location))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop as-match (let-logger-properties 1))
;; End:
