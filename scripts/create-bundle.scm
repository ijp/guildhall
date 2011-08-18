;;; show-bundle.scm --- Dorodango for Guile

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

(define-module (scripts create-bundle)
  #:use-module (rnrs)
  #:use-module (ice-9 match)
  #:use-module (guild spells pathname)
  #:use-module (guild cli)
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild private utils)
  #:use-module (guild package)
  #:use-module (guild bundle)
  #:use-module (guild actions)
  #:use-module (guild ui formatters))

(define %summary "Create a bundle.")
(define %synopsis "guild create-bundle DIRECTORY...")
(define %help
  "Create a bundle.

  -o, --output=FILE    Bundle filename.
  -d, --directory=DIR  Output directory when using implicit filename.
      --append-version=V  Append V to each package's version.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define %mod (current-module))
(define (main . args)
  (define output-filename #f)
  (define output-directory #f)
  (define version '())
  (call-with-parsed-options
      %mod args
      (list
       (make-option/arg '("output" #\o)
                        (lambda (val) (set! output-filename val)))
       (make-option/arg '("directory" #\d)
                        (lambda (val)
                          (set! output-directory (pathname-as-directory val))))
       (make-option/arg '("append-version")
                        (lambda (val)
                          (set! version (string->package-version val)))))
    (lambda (operands config)
      (define (compute-bundle-filename packages)
        (match packages
          (()
           (fatal "all package lists have been empty."))
          ((package)
           (package->string package "_"))
          (_
           (fatal "multiple packages found and no bundle name specified."))))
      (let ((directories (if (null? operands)
                             (list (make-pathname #f '() #f))
                             (map pathname-as-directory operands))))
        (let ((pkg-list-files (find-pkg-list-files directories))
              (need-rewrite? (not (null? version))))
          (when (null? pkg-list-files)
            (fatal (cat "no package lists found in or below "
                        (fmt-join dsp-pathname pkg-list-files ", ") ".")))
          (let* ((packages-list (read-package-lists pkg-list-files version))
                 (output
                  (or output-filename
                      (->namestring
                       (pathname-with-file
                        (or output-directory (make-pathname #f '() #f))
                        (compute-bundle-filename (apply append packages-list)))))))
            (create-bundle output
                           (map (lambda (pathname)
                                  (pathname-with-file pathname #f))
                                pkg-list-files)
                           packages-list
                           need-rewrite?))))))
  (exit 0))
