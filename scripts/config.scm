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

(define-module (scripts config)
  #:autoload (scripts help) (show-usage)
  #:use-module (guildhall cli)
  #:use-module (guildhall cli config)
  #:use-module (guildhall ext fmt)
  #:use-module (guildhall spells pathname)
  #:use-module (guildhall private utils)
  #:use-module (guildhall config)
  #:use-module (guildhall destination)
  #:use-module (guildhall package)
  #:use-module (guildhall repository)
  #:use-module (guildhall ui formatters))

(define %summary "Show configuration.")
(define %synopsis "config\nconfig destination PACKAGE CATEGORY [FILENAME]")
(define %help "
  -c, --config=FILE    Use configuration file FILE, instead of the
                       default.
      --no-config      Do not read a configuration file.
      --help           Print this help message.
      --version        Print version information.
")

(define* (show-destination dest package category #:optional pathname)
  (fmt #t (dsp-pathname
           (destination-pathname dest
                                 (string->package package "=")
                                 (string->symbol category)
                                 (if pathname
                                     (->pathname pathname)
                                     (make-pathname #f '() #f)))) 
       "\n"))

(define (dsp-config config)
  (define (dsp-config-item item)
    (let ((dest (config-item-destination item)))
      (cat (if (eq? (destination-name dest) (config-default-name config))
               "*"
               "-")
           " name: " (destination-name dest) "\n"
           "  prefix: " (->namestring (destination-prefix dest)) "\n" 
           "  database: "
           (->namestring (config-item-database-location item)) "\n"
           "  cache-directory: "
           (->namestring (config-item-cache-directory item)) "\n"
           "  repositories:\n"
           (fmt-indented "    " (fmt-join dsp-repository
                                          (config-item-repositories item))))))
  (define (dsp-repository repo)
    (cat "- " (repository-name repo) ": " (repository-location repo) "\n"))
  (cat "destinations:\n"
       (fmt-indented "  " (fmt-join dsp-config-item (config-items config)))))

(define %mod (current-module))
(define (main . args)
  (call-with-parsed-options/config %mod args '()
    (lambda (args config)
      (let ((n-operands (length args)))
        (cond
         ((zero? n-operands)
          (fmt #t (dsp-config config)))
         ((and (equal? (car args) "destination")
               (or (= n-operands 3) (= n-operands 4)))
          (apply show-destination
                 (config-item-destination (config-default-item config))
                 (cdr args)))
         (else
          (with-output-to-port (current-error-port)
            (lambda ()
              (display "unexpected arguments: ")
              (display (string-join args " "))
              (newline)
              (show-usage %mod "config")
              (exit 1))))))))
  (exit 0))
