;;; doro.sps --- Dorodango package manager

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

;; This is the command-line interface to dorodango.

;;; Code:
#!r6rs

(import (except (rnrs) file-exists? delete-file)
        (srfi :8 receive)
        (only (srfi :13) string-null? string-trim-both)
        (srfi :67 compare-procedures)
        (spells alist)
        (spells match)
        (spells fmt)
        (spells foof-loop)
        (spells pathname)
        (spells filesys)
        (spells sysutils)
        (spells define-values)
        (rename (spells args-fold)
                (option %option))
        (spells logging)
        (spells tracing)
        (only (spells record-types) define-record-type*)
        (dorodango private utils)
        (dorodango package)
        (dorodango database)
        (dorodango destination)
        (dorodango bundle)
        (only (dorodango solver) logger:dorodango.solver)
        (dorodango config)
        (dorodango ui cmdline))


;;; Command-line processing

(define-record-type* option-info
  (make-option-info %option metavar help)
  ())

(define (option-info-names opt-info)
  (option-names (option-info-%option opt-info)))

(define (%option-proc proc)
  (lambda (option name arg . seeds)
    (apply proc name arg seeds)))

(define (option names arg-info help proc)
  (define (info arg-required? arg-optional? metavar)
    (make-option-info (%option names
                               arg-required?
                               arg-optional?
                               (%option-proc proc))
                      metavar
                      help))
  (match arg-info
    ('#f
     (info #f #f #f))
    ((? symbol? metavar)
     (info #t #f metavar))))

(define (help-%option command)
  (%option
   '("help" #\h) #f #f
   (lambda (option name arg vals)
     (values #t (acons 'run
                       (lambda (vals)
                         (fmt #t (dsp-help command))
                         '())
                       vals)))))

(define (dsp-option-name name)
  (cat (if (string? name) "--" "-") name))

(define (dsp-opt-info/left-side opt-info)
  (cat (fmt-join dsp-option-name (option-info-names opt-info) ", ")
       (cond ((option-info-metavar opt-info)
              => (lambda (metavar)
                   (cat " " (string-upcase (symbol->string metavar)))))
             (else
              ""))))

(define (dsp-help command)
  (cat "doro "(apply-cat (command-synopsis command)) "\n"
       (apply-cat (command-description command)) "\n"
       "Options:\n"
       (dsp-listing "  " (append
                          (map (lambda (opt-info)
                                 (dsp-opt-info/left-side opt-info))
                               (command-options command))
                          '("--help"))
                    "  " (append (map option-info-help (command-options command))
                                 '("Show this help and exit")))))

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
               ;; TODO: use `columnar' and `wrap-lines' here
               (cat (pad left-width (dsp left)) (cat separator right "\n")))
             left-sides right-items))
       st))))


;;; Commands

(define %commands '())

(define (command-list)
  (reverse %commands))

(define-record-type* command
  (make-command name description synopsis options handler)
  ())

(define (find-command name)
  (find (lambda (command)
          (eq? name (command-name command)))
        %commands))

(define-syntax define-command
  (syntax-rules (description synopsis options handler)
    ((_ name
        (description description-item ...)
        (synopsis synopsis-item ...)
        (options option-item ...)
        (handler proc))
     (define-values ()
       (set! %commands (cons (make-command 'name
                                           (list description-item ...)
                                           (list synopsis-item ...)
                                           (list option-item ...)
                                           proc)
                             %commands))))))

(define (arg-pusher name)
  (lambda (option-name arg vals)
    (values #f (apush name arg vals))))

(define (value-setter name value)
  (lambda (option-name arg vals)
    (values #f (acons name value vals))))

(define bundle-option
  (option '("bundle" #\b) 'bundle
          "Additionally consider packages from BUNDLE"
          (arg-pusher 'bundles)))

(define no-depends-option
  (option '("no-depends") #f
          "Ignore dependencies"
          (value-setter 'no-depends? #f)))

(define (parse-package-string s)
  (values (string->symbol s) #f))

(define (find-db-items db packages)
  (loop ((for package (in-list packages))
         (for result
              (listing
               (receive (name version) (parse-package-string package)
                 (database-lookup db name version)))))
    => (reverse result)))

(define (bail-out formatter)
  (fmt (current-error-port) (cat formatter "\n"))
  (exit 1))

(define (opt-ref/list vals key)
  (reverse (or (assq-ref vals key) '())))



(define-command list
  (description "List packages")
  (synopsis "list")
  (options (option '("all") #f
                   "Also show available packages"
                   (value-setter 'all? #t))
           bundle-option)
  (handler
   (lambda (vals)
     (let ((all? (assq-ref vals 'all?))
           (db (config->database (assq-ref vals 'config))))
       (database-add-bundles! db (opt-ref/list vals 'bundles))
       (loop ((for package items (in-database db (sorted-by symbol<?))))
         (cond (all?
                (fmt #t (fmt-join/suffix dsp-db-item/short items "\n")))
               ((find database-item-installed? items)
                => (lambda (installed)
                     (fmt #t (dsp-db-item/short installed) "\n")))))))))


(define-command show
  (description "Show package information")
  (synopsis "show [--bundle BUNDLE]... PACKAGE...")
  (options bundle-option)
  (handler
   (lambda (vals)
     (let ((packages (opt-ref/list vals 'operands))
           (db (config->database (assq-ref vals 'config))))
       (database-add-bundles! db (opt-ref/list vals 'bundles))
       (loop ((for item (in-list (find-db-items db packages))))
         (fmt #t (dsp-db-item item)))))))


(define-command show-bundle
  (description "Show bundle contents")
  (synopsis "show-bundle BUNDLE...")
  (options)
  (handler
   (lambda (vals)
     (loop ((for bundle-location (in-list (opt-ref/list vals 'operands))))
       (let ((bundle (open-input-bundle bundle-location)))
         (fmt #t (dsp-bundle bundle)))))))


(define (select-package db package-string)
  (receive (name version) (parse-package-string package-string)
    (let ((item (database-lookup db name version)))
      (cond ((not item)
             (bail-out (cat "Couldn't find any package matching \""
                            package-string "\"")))
            (else
             (database-item-package item))))))

(define (install-command vals)
  (let ((bundle-locations (opt-ref/list vals 'bundles))
        (packages (opt-ref/list vals 'operands))
        (no-depends? (assq-ref vals 'no-depends?))
        (db (config->database (assq-ref vals 'config))))
    (database-add-bundles! db bundle-locations)
    (loop ((for package (in-list packages))
           (for to-install (listing (select-package db package))))
      => (cond (no-depends?
                (loop ((for package (in-list to-install)))
                  (database-install! db package)))
               (else
                (apply-actions db to-install '()))))))

(define-command install
  (description "Install new packages")
  (synopsis "install [--bundle BUNDLE]... PACKAGE...")
  (options bundle-option no-depends-option)
  (handler install-command))


(define (remove-command vals)
  (let ((packages (opt-ref/list vals 'operands))
        (db (config->database (assq-ref vals 'config))))
    (loop ((for package-name (in-list packages)))
      (unless (database-remove! db (string->symbol package-name))
        (message "Package " package-name " was not installed.")))))

(define-command remove
  (description "Remove packages")
  (synopsis "remove PACKAGE...")
  (options)
  (handler remove-command))


;;; Entry point

(define (process-command-line command cmd-line seed-vals)
  (define (unrecognized-option option name arg vals)
    (error 'process-command-line "unrecognized option" name))
  (define (process-operand operand vals)
    (apush 'operands operand vals))
  (let ((vals (args-fold* cmd-line
                          (cons (help-%option command)
                                (map option-info-%option
                                     (command-options command)))
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

(define (dsp-usage)
  (cat "dorodango v0.0\n"
       "Usage: doro COMMAND OPTION... ARG...\n"
       "\n"
       (wrap-lines
        "doro is a simple command-line interface for downloading, "
        "installing and inspecting packages containing R6RS libraries.")
       "\n"
       "Commands:\n"
       (dsp-listing "  " (map command-name (command-list))
                    "  " (map (lambda (command)
                                 (apply-cat (command-description command)))
                               (command-list)))
       "\n\n"
       "Use \"doro COMMAND --help\" to get more information about COMMAND.\n"
       (pad/both 72 "This doro has Super Ball Powers.")))

(define (home-pathname pathname)
  (pathname-join (pathname-as-directory
                  (lookup-environment-variable "HOME"))
                 pathname))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "dorodango") "config.scm")))

(define (default-database-directory)
  (home-pathname '((".local" "var" "lib" "dorodango"))))

(define (default-destination)
  (make-fhs-destination 'default (home-pathname '((".local")))))

(define (default-config)
  (make-config 'default (list (default-destination))))

(define (read-default-config)
  (let ((pathname (default-config-location)))
    (if (file-exists? pathname)
        (call-with-input-file (->namestring pathname)
          read-config)
        (default-config))))

(define (config->database config)
  (open-database (default-database-directory)
                 (config-default-destination config)
                 '()))

(define (main argv)
  (set-logger-properties!
   logger:dorodango
   `((threshold info)
     (handlers
      ,(lambda (entry)
         (default-log-formatter entry (current-output-port))))))
  (set-logger-properties!
   logger:dorodango.solver
   `((threshold warning)
     (handlers
      ,(lambda (entry)
         (default-log-formatter entry (current-output-port))))))
  (match argv
    ((self)
     (fmt #t (dsp-usage)))
    ((self "--help" . rest)
     (fmt #t (dsp-usage)))
    ((self command . rest)
     (cond ((find-command (string->symbol command))
            => (lambda (command)
                 (let ((config (read-default-config)))
                   (process-command-line command
                                         rest
                                         `((operands . ())
                                           (config . ,config))))))
           (else
            (error 'main "unknown command" command))))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1) (make-finite-type-vector 3))
;; End:
