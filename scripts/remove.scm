;;; remove.scm --- Dorodango for Guile

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

(define-module (scripts remove)
  #:use-module (rnrs)
  #:use-module (sigil cli)
  #:use-module (sigil cli ui)
  #:use-module (sigil ext fmt)
  #:use-module (sigil ext foof-loop)
  #:use-module (sigil private utils)
  #:use-module (sigil hooks)
  #:use-module (sigil database)
  #:use-module (sigil package)
  #:use-module (sigil ui cmdline dependencies)
  #:use-module (sigil ui formatters)
  #:use-module (ice-9 receive))

(define %summary "Remove installed packages.")
(define %synopsis "remove PACKAGE...")
(define %help "
      --no-depends     Ignore dependencies.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define %mod (current-module))
(define (main . args)
  (define no-depends? #f)
  (call-with-parsed-options/config+db/ui
      %mod args
      (list (make-option
             '("no-depends")
             (lambda () (set! no-depends? #t))))
    (lambda (packages config db)
      (define (not-installed-error package-name)
        (fatal (cat "package `" package-name "' is not installed.")))
      (cond
       (no-depends?
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
            (not-installed-error package-string)))))))
  (exit 0))
