;;; list-packages.scm --- Dorodango for Guile

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

(define-module (scripts list-packages)
  #:use-module (rnrs)
  #:use-module (guildhall cli)
  #:use-module (guildhall cli db)
  #:use-module (guildhall ext fmt)
  #:use-module (guildhall ext foof-loop)
  #:use-module (guildhall private utils)
  #:use-module (guildhall database)
  #:use-module (guildhall package)
  #:use-module (guildhall ui formatters))

(define %summary "List available packages.")
(define %synopsis "list-packages")
(define %help "
  -a, --all            Show all packages, including uninstalled but
                       available ones.
  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
  -b, --bundle=BUNDLE  Temporarily add BUNDLE's contents to the package
                       database.
      --help           Print this help message.
      --version        Print version information.
")

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

(define %mod (current-module))
(define (main . args)
  (define all? #f)
  (define bundles '())
  (call-with-parsed-options/config+db
      %mod args
      (list
       (make-option
        '("all" #\a)
        (lambda () (set! all? #t)))
       (make-option/arg
        '("bundle" #\b)
        (lambda (arg) (set! bundles (append bundles (list arg))))))
    (lambda (args config db)
      (database-add-bundles! db bundles)
      (loop ((for package items (in-database db (sorted-by symbol<?))))
        (cond (all?
               (fmt #t (fmt-join/suffix dsp-db-item/short items "\n")))
              ((find database-item-installed? items)
               => (lambda (installed)
                    (fmt #t (dsp-db-item/short installed) "\n")))))))
  (exit 0))
