;;; install.scm --- Dorodango for Guile

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

(define-module (scripts install)
  #:use-module (rnrs)
  #:use-module (guild ui guild)
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild private utils)
  #:use-module (guild hooks)
  #:use-module (guild database)
  #:use-module (guild package)
  #:use-module (guild ui)
  #:use-module (guild ui cmdline dependencies)
  #:use-module (guild ui formatters)
  #:use-module (ice-9 receive))

(define %summary "Install new packages.")
(define %synopsis "guild install [--bundle=BUNDLE]... PACKAGE...")
(define %help
  "Install new packages.

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

(define %mod (current-module))
(define (main . args)
  (define bundles '())
  (define no-depends? #f)
  (define assume-yes? #f)
  (define non-interactive? #f)
  (call-with-parsed-options+db
      %mod args
      (list (make-option/arg
             '("bundle" #\b)
             (lambda (arg) (set! bundles (append bundles (list arg)))))
            (make-option
             '("no-depends")
             (lambda () (set! no-depends? #t)))
            (make-option
             '("yes" #\y)
             (lambda () (set! assume-yes? #t)))
            (make-option
             '("non-interactive" #\n)
             (lambda () (set! non-interactive? #t))))
    (lambda (packages config db)
      (database-add-bundles! db bundles)
      (call-with-cmdline-ui assume-yes? non-interactive?
        (lambda ()
          (loop ((for package (in-list packages))
                 (for to-install (listing (select-package/string db package))))
            => (cond (no-depends?
                      (install/no-depends db to-install))
                     (else
                      (apply-actions db to-install '()))))))))
  (exit 0))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1) (call-with-parsed-options 3))
;; End:
