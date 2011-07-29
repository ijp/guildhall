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

(define %summary "Upgrade all packages.")
(define %synopsis "guild upgrade")
(define %help
  "Upgrade all packages.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define %mod (current-module))
(define (main . args)
  (call-with-values (lambda () (parse-options mod args))
    (lambda (packages config)
      (call-with-database* config
        (lambda (db)
          (define (select-upgrade items)
            (let ((item (car items)))
              (and item
                   (exists database-item-installed? items)
                   (not (database-item-installed? item))
                   (database-item-package item))))
          (loop ((for package-name items (in-database db))
             (for to-upgrade (listing (select-upgrade items) => values)))
        => (apply-actions db to-upgrade '()))))))
  (exit 0))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1))
;; End:
