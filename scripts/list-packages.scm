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
#!r6rs

(define-module (scripts list-packages)
  #:use-module (rnrs)
  #:use-module (guild ui guild)
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild private utils)
  #:use-module (guild database)
  #:use-module (guild package)
  #:use-module (guild ui formatters))

(define %summary "List packages.")
(define %synopsis "guild list-packages")
(define %help
  "List available packages.

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
  (call-with-values
      (lambda ()
        (parse-options
         %mod args
         (make-option
          '("all" #\a)
          (lambda (arg) (set! all? #t)))
         (make-option
          '("bundle" #\b)
          (lambda (arg) (set! bundles (append bundles (list arg))))
          #:has-arg 'required)))
    (lambda (args config)
      (call-with-database* config
        (lambda (db)
          (database-add-bundles! db bundles)
          (loop ((for package items (in-database db (sorted-by symbol<?))))
            (cond (all?
                   (fmt #t (fmt-join/suffix dsp-db-item/short items "\n")))
                  ((find database-item-installed? items)
                   => (lambda (installed)
                        (fmt #t (dsp-db-item/short installed) "\n")))))))))
  (exit 0))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1))
;; End:
