;;; symlink-bundle.scm --- Dorodango for Guile

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

(define-module (scripts symlink-bundle)
  #:use-module (rnrs)
  #:use-module (ice-9 match)
  #:use-module (guild ui guild)
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild ext foof-loop nested)
  #:use-module (guild private utils)
  #:use-module (guild actions)
  #:use-module (guild database)
  #:use-module (guild package)
  #:use-module (guild bundle)
  #:use-module (guild ui formatters))

(define %summary "Create symbolic links for a bundle.")
(define %synopsis "guild symlink-bundle BUNDLE-DIRECTORY TARGET-DIRECTORY")
(define %help
  "Create symbolic links for a bundle.

      --force             Force operation.
      --deep              Only symlink files.
      --include=PACKAGES  Only consider PACKAGES.
      --exclude=PACKAGES  Don't consider PACKAGES.

  PACKAGES should be a space or comma separated list.

  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define (string->package-list string)
  (map string->symbol (string-tokenize
                       string
                       (char-set-complement (string->char-set " ,")))))

(define %mod (current-module))
(define (main . args)
  (define force? #f)
  (define deep? #f)
  (define only #f)
  (define except #f)
  (call-with-parsed-options
   %mod args
   (list
    (make-option '("force")
                 (lambda () (set! force? #t)))
    (make-option '("deep")
                 (lambda () (set! deep? #t)))
    (make-option/arg '("include")
                     (lambda (val)
                       (set! only (string->package-list val))))
    (make-option/arg '("exclude")
                     (lambda (val)
                       (set! except (string->package-list val)))))
   (lambda (args config)
     (match args
       ((bundle-directory target-directory)
        (symlink-bundle bundle-directory
                        target-directory
                        force?
                        deep?
                        (lambda (package)
                          (cond ((and only except)
                                 (and (memq (package-name package) only)
                                      (not (memq (package-name package) except))))
                                (only
                                 (memq (package-name package) only))
                                (except
                                 (not (memq (package-name package) except)))
                                (else
                                 #t)))))
       (_
        (fatal "`symlink' expects two arguments")))))
  
  (exit 0))
