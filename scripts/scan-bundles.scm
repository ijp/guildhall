;;; scan-bundle.scm --- Dorodango for Guile

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

(define-module (scripts scan-bundles)
  #:use-module (rnrs)
  #:use-module (ice-9 match)
  #:use-module (sigil cli)
  #:use-module (sigil ext fmt)
  #:use-module (sigil ext foof-loop)
  #:use-module (sigil ext foof-loop nested)
  #:use-module (sigil private utils)
  #:use-module (sigil actions)
  #:use-module (sigil database)
  #:use-module (sigil package)
  #:use-module (sigil bundle)
  #:use-module (sigil ui formatters))

(define %summary "Scan directories for bundles.")
(define %synopsis "scan-bundles DIRECTORY...")
(define %help "
  -o, --output=FILE    Write the scan results to FILE.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define %mod (current-module))
(define (main . args)
  (define output (current-output-port))
  (call-with-parsed-options
      %mod args
      (list
       (make-option/arg
        '("output" #\o)
        (lambda (val) (set! output (open-file-output-port val)))))
    (lambda (args)
      (iterate! (for directory (in-list args))
          (for entry (in-list (scan-bundles-in-directory directory directory)))
        (match entry
          ((package . bundle-pathname)
           (fmt output
                (pretty/unshared
                 (package->form
                  (package-with-property
                   package
                   'location
                   (list (pathname->location bundle-pathname)))))))))))
  
  (exit 0))
