;;; upgrade.scm --- Dorodango for Guile

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

(define-module (scripts upgrade)
  #:use-module (rnrs)
  #:use-module (guildhall cli ui)
  #:use-module (guildhall ext foof-loop)
  #:use-module (guildhall database)
  #:use-module (guildhall package)
  #:use-module (guildhall ui cmdline dependencies)
  #:use-module (guildhall ui formatters)
  #:use-module (ice-9 receive))

(define %summary "Upgrade all packages.")
(define %synopsis "upgrade")
(define %help "
  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define %mod (current-module))
(define (main . args)
  (call-with-parsed-options/config+db/ui %mod args '()
    (lambda (packages config db)
      (define (select-upgrade items)
        (let ((item (car items)))
          (and item
               (or-map database-item-installed? items)
               (not (database-item-installed? item))
               (database-item-package item))))
      (loop ((for package-name items (in-database db))
             (for to-upgrade (listing (select-upgrade items) => values)))
        => (apply-actions db to-upgrade '()))))
  (exit 0))
