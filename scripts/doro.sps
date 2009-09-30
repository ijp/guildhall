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
        (spells alist)
        (spells match)
        (spells fmt)
        (spells foof-loop)
        (spells gc)
        (spells pathname)
        (spells filesys)
        (spells sysutils)
        (spells lazy)
        (spells lazy-streams)
        (rename (spells args-fold)
                (option %option))
        (spells logging)
        (spells tracing)
        (dorodango private utils)
        (dorodango database)
        (dorodango destination)
        (dorodango config)
        (dorodango bundle)
        (dorodango package)
        (dorodango inventory))


;;; Command-line processing


(define-record-type option-info
  (fields %option metavar help))

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

(define (help-%option synopsis options)
  (%option
   '("help" #\h) #f #f
   (lambda (option name arg vals)
     (values #t (acons 'run
                       (lambda (vals)
                         (fmt #t (dsp-help synopsis options))
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

(define (dsp-help synopsis opt-infos)
  (lambda (st)
    (let* ((left-sides
            (append
             (map (lambda (opt-info)
                    (fmt #f (cat "  " (dsp-opt-info/left-side opt-info))))
                  opt-infos)
             '("  --help")))
           (left-width (fold-left max 0 (map string-length left-sides)))
           (right-sides (append (map option-info-help opt-infos)
                                '("Show this help and exit"))))
      ((cat
        (apply-cat synopsis)
        "\n"
        "Options:\n"
        (apply-cat
         (map (lambda (left right)
                (columnar (with-width left-width left) "   " (dsp right)))
              left-sides right-sides)))
       st))))


;;; Commands

(define %commands '())

(define (command-list)
  (reverse %commands))

(define-syntax define-command
  (syntax-rules (description synopsis options handler)
    ((_ name
        (description description-item ...)
        (synopsis synopsis-item ...)
        (options option-item ...)
        (handler proc))
     (set! %commands (cons (list 'name
                                 (list description-item ...)
                                 (list synopsis-item ...)
                                 (list option-item ...)
                                 proc)
                           %commands)))))

(define command-name car)
(define command-description cadr)

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

(define (parse-package-string s)
  (values (string->symbol s) #f))

(define (find-db-items db packages inventory?)
  (let ((options (if inventory?
                     (database-search-options inventories)
                     (database-search-options))))
    (loop ((for package (in-list packages))
           (for result
                (appending-reverse
                 (receive (name version) (parse-package-string package)
                   (database-search db name version options)))))
      => (reverse result))))

(define (bail-out formatter)
  (fmt (current-error-port) (cat formatter "\n"))
  (exit 1))

(define (opt-ref/list vals key)
  (reverse (or (assq-ref vals key) '())))

(define-command show
  (description "Show information about packages")
  (synopsis "show [--bundle BUNDLE]... PACKAGE...")
  (options bundle-option
           (option '("inventory") #f
                   "Show the package inventories"
                   (value-setter 'inventory? #t)))
  (handler
   (lambda (vals)
     (let ((bundle-locations (opt-ref/list vals 'bundles))
           (packages (opt-ref/list vals 'operands))
           (inventory? (assq-ref vals 'inventory?))
           (db (config->database (assq-ref vals 'config))))
       (database-add-bundles! db bundle-locations)
       (loop ((for item (in-list (find-db-items db packages inventory?))))
         (fmt #t (dsp-database-item item)))))))

(define (install-command vals)
  (let ((bundle-locations (opt-ref/list vals 'bundles))
        (packages (opt-ref/list vals 'operands))
        (db (config->database (assq-ref vals 'config))))
    (database-add-bundles! db bundle-locations)
    (loop ((for package (in-list packages)))
      (receive (name version) (parse-package-string package)
        (let ((items (database-search db name version)))
          (cond ((null? items)
                 (bail-out (cat "Couldn't find any package matching \""
                                package "\"")))
                ((not (null? (cdr items)))
                 (bail-out (cat "Multiple versions of package found:\n"
                                "  " (fmt-join dsp-item-identifier items " ")
                                "Please specify a version as well.")))
                (else
                 (database-install! db (database-item-package (car items))))))))))

(define-command install
  (description "Install new packages")
  (synopsis "install [--bundle BUNDLE]... PACKAGE...")
  (options bundle-option)
  (handler install-command))

(define (remove-command vals)
  (let ((packages (opt-ref/list vals 'operands))
        (db (config->database (assq-ref vals 'config))))
    (loop ((for package (in-list packages)))
      (receive (name version) (parse-package-string package)
        (loop ((for item (in-list (database-search db name version))))
          (when (database-item-installed? item)
            (database-remove! db (database-item-package item))))))))

(define-command remove
  (description "Remove packages")
  (synopsis "remove PACKAGE...")
  (options)
  (handler remove-command))


;;; Formatting combinators

(define (dsp-package pkg)
  (cat "Name: " (package-name pkg) "\n"
       (if (null? (package-version pkg))
           fmt-null
           (cat "Version: " (dsp-version (package-version pkg)) "\n"))
       (fmt-join (lambda (category)
                   (let ((inventory (package-category-inventory pkg category)))
                     (if (inventory-empty? inventory)
                         fmt-null
                         (cat "Inventory: " category "\n"
                              (dsp-inventory inventory)))))
                 (package-categories pkg))))

(define (dsp-database-item item)
  (dsp-package (database-item-package item)))

(define (dsp-version version)
  (fmt-join (lambda (part)
              (fmt-join dsp part "."))
            version
            "-"))

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


(define (dsp-item-identifier item)
  (dsp-package-identifier (database-item-package item)))

(define (dsp-package-identifier package)
  (cat (package-name package) "-" (dsp-version (package-version package))))


;;; Entry point

(define (process-command-line cmd-line synopsis options processor seed-vals)
  (define (unrecognized-option option name arg vals)
    (error 'process-command-line "unrecognized option" name))
  (define (process-operand operand vals)
    (apush 'operands operand vals))
  (let ((vals (args-fold* cmd-line
                          (cons (help-%option synopsis options)
                                (map option-info-%option options))
                          unrecognized-option
                          process-operand
                          seed-vals)))
    (cond ((assq-ref vals 'run)
           => (lambda (run)
                (run vals)))
          (else
           (processor vals)))))

(define (dsp-usage)
  (cat "dorodango v0.0\n"
       "Usage: doro COMMAND OPTION... ARG...\n"
       "\n"
       (wrap-lines
        "doro is a simple command-line interface for downloading, "
        "installing and inspecting packages containing R6RS libraries.")
       "\n"
       "Commands:\n"
       (fmt-join (lambda (command)
                   (cat "   " (command-name command)
                        " - " (apply-cat (command-description command))))
                 (command-list)
                 "\n")
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
  (match argv
    ((self)
     (fmt #t (dsp-usage)))
    ((self "--help" . rest)
     (fmt #t (dsp-usage)))
    ((self command . rest)
     (cond ((assq (string->symbol command) %commands)
            => (lambda (command)
                 (let ((config (read-default-config)))
                   (match command
                     ((name description synopsis options processor)
                      (process-command-line rest
                                            (append '("doro ") synopsis)
                                            options
                                            processor
                                            `((operands . ())
                                              (config . ,config))))))))
           (else
            (error 'main "unknown command" command))))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
