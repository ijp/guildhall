;;; clean.scm --- Dorodango for Guile

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

(define-module (scripts clean)
  #:autoload (scripts help) (show-usage)
  #:use-module (sigil cli)
  #:use-module (sigil cli db)
  #:use-module (sigil database))

(define %summary "Clean the package cache.")
(define %synopsis "clean")
(define %help "
  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define %mod (current-module))
(define (main . args)
  (call-with-parsed-options/config+db %mod args '()
    (lambda (args config db)
      (cond
       ((null? args)
        (database-clear-cache! db))
       (else
        (with-output-to-port (current-error-port)
          (lambda ()
            (display "unexpected arguments: ")
            (display (string-join args " "))
            (newline)
            (show-usage %mod "clean")
            (exit 1)))))))
  (exit 0))
