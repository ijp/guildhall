;;; show.scm --- Dorodango for Guile

;; Copyright (C) 2011 Free Software Foundation, Inc.
;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>
;; Author: Andy Wingo <wingo@pobox.com>

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

(define-module (scripts show)
  #:use-module (rnrs)
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild spells pathname)
  #:use-module (guild spells filesys)
  #:use-module (guild private utils)
  #:use-module (guild database)
  #:use-module (guild package)
  #:use-module (guild config)
  #:use-module (guild ui formatters)
  #:use-module (srfi srfi-37))

(define %summary "Show package information.")
(define %synopsis "guild show [--bundle=BUNDLE]... PACKAGE...")
(define %help
  "Show package information.

  -b, --bundle=BUNDLE  Temporarily add BUNDLE's contents to the package
                       database.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define* (show-usage #:optional (port (current-output-port)))
  (display "Usage: " port)
  (display %synopsis port)
  (newline port))

(define* (show-help #:optional (port (current-output-port)))
  (show-usage port)
  (display %help port))

(define options
  (list
   (option '("bundle" #\b) #t #f
           (lambda (opt name val args config bundles)
             (values args config (append bundles (list val)))))
   (option '("help" #\h) #f #f
           (lambda (opt name val args config bundles)
             (show-help)
             (exit 0)))
   (option '("config" #\c) #t #f
           (lambda (opt name val args config bundles)
             (values args val bundles)))
   (option '("no-config") #f #f
           (lambda (opt name val args config bundles)
             (values args #f bundles)))))

(define (parse-options cmd-line . seeds)
  (apply args-fold cmd-line options
         (lambda (opt name arg . seeds)
           (display "unrecognized option: " (current-error-port))
           (display name (current-error-port))
           (newline (current-error-port))
           (show-usage (current-error-port))
           (exit 0))
         (lambda (operand args . seeds)
           (apply values (cons operand args) seeds))
         '() seeds))

(define* (open-database* config #:key
                         (destination (config-default-name config))
                         (repositories '()))
  (let* ((item (if destination
                   (or (config-ref config destination)
                       (fatal (cat "no such destination configured: " destination)))
                   (config-default-item config)))
         (location (config-item-database-location item)))
    (guard (c ((database-locked-error? c)
               (fatal (cat "database locked: " (dsp-pathname location)))))
      (open-database location
                     (config-item-destination item)
                     (append repositories (config-item-repositories item))
                     (config-item-cache-directory item)))))

(define (read-config/guard pathname)
  (guard (c ((i/o-file-does-not-exist-error? c)
             (fatal (cat "specified config file `"
                         (dsp-pathname pathname) "' does not exist."))))
    (call-with-input-file (->namestring pathname)
      read-config)))

(define (call-with-database* config proc)
  (call-with-database
      (open-database* (if config
                          (read-config/guard config)
                          (default-config)))
    (lambda (db)
      (guard (c ((error? c)
                 (close-database db)
                 (raise c)))
        (proc db)))))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "dorodango") "config.scm")))

(define (dsp-db-item item)
  (dsp-package (database-item-package item)
               (cat "Status: " (database-item-state item) "\n")))

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
               (call-with-values (lambda () (parse-package-string package))
                 (lambda (name version)
                   (if version
                       (or (and=> (database-lookup db name version) list)
                           '())
                       (database-items db name)))))))
    => (reverse result)))

(define (process-command-line cmd-line)
  (call-with-values (lambda () (parse-options cmd-line (default-config-location) '()))
    (lambda (packages config bundles)
      (call-with-database* config
        (lambda (db)
          (database-add-bundles! db bundles)
          (fmt #t (fmt-join dsp-db-item
                            (find-db-items db packages)
                            "\n")))))))

(define (main . args)
  (process-command-line args)
  (exit 0))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1))
;; End:
