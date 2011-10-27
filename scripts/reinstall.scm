;;; reinstall.scm --- Dorodango for Guile

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

(define-module (scripts reinstall)
  #:use-module (srfi srfi-34)
  #:use-module (guildhall cli)
  #:use-module (guildhall cli ui)
  #:use-module (guildhall ext fmt)
  #:use-module (guildhall ext foof-loop)
  #:use-module (guildhall private utils)
  #:use-module (guildhall hooks)
  #:use-module (guildhall database)
  #:use-module (guildhall package)
  #:use-module (guildhall ui)
  #:use-module (guildhall ui cmdline dependencies)
  #:use-module (guildhall ui formatters)
  #:use-module (ice-9 receive))

(define %summary "Reinstall packages.")
(define %synopsis "reinstall [--bundle=BUNDLE]... PACKAGE...")
(define %help "
  -b, --bundle=BUNDLE  Additionally consider packages from BUNDLE.
      --no-depends     Ignore dependencies.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define (parse-package-string s)
  (cond ((maybe-string->package s "=")
         => (lambda (package)
              (values (package-name package)
                      (package-version package))))
        (else
         (values (string->symbol s) #f))))

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

(define (reinstall/no-depends db to-install)
  (loop ((for package (in-list to-install)))
    (database-remove! db (package-name package))
    (cond ((database-unpack! db package)
           (guard (c ((hook-runner-exception? c)
                      (fmt (current-error-port) (dsp-hook-runner-exception c))))
             (database-setup! db (package-name package))))
          (else
           (let ((db-package
                  (database-lookup db (package-name package) 'installed)))
             (message "Package " package-name
                      " already at version " (package-version db-package)))))))

(define %mod (current-module))
(define (main . args)
  (define bundles '())
  (define no-depends? #f)
  (call-with-parsed-options/config+db/ui
      %mod args
      (list (make-option/arg
             '("bundle" #\b)
             (lambda (arg) (set! bundles (append bundles (list arg)))))
            (make-option
             '("no-depends")
             (lambda () (set! no-depends? #t))))
    (lambda (packages config db)
      (database-add-bundles! db bundles)
      (loop ((for package (in-list packages))
             (for to-install (listing (select-package/string db package))))
        => (cond (no-depends?
                  (reinstall/no-depends db to-install))
                 (else
                  (apply-actions db to-install '()
                                 (action-options reinstall)))))))
  (exit 0))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1) (call-with-parsed-options 3))
;; End:
