;;; guild.scm --- Command-line UI library

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

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

;;; Code:
(define-module (guild ui guild)
  #:use-module (rnrs)
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild spells pathname)
  #:use-module (guild spells filesys)
  #:use-module (guild private utils)
  #:use-module (guild database)
  #:use-module (guild package)
  #:use-module (guild config)
  #:use-module (guild ui formatters)
  #:use-module (srfi srfi-37)
  #:export (show-usage make-option parse-options call-with-database*))

(define* (show-usage mod #:optional (port (current-output-port)))
  (display "Usage: " port)
  (display (module-ref mod '%synopsis) port)
  (newline port))

(define* (show-help mod #:optional (port (current-output-port)))
  (show-usage port)
  (display (module-ref mod '%help) port))

(define* (make-option names parser #:key has-arg)
  (option names
          (case has-arg
            ((required) #t)
            ((optional #f) #f)
            (else (error "unexpected #:has-arg" has-arg)))
          (case has-arg
            ((optional) #t)
            (else #f))
          (lambda (opt name val args config)
            (parser val)
            (values args config))))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "dorodango") "config.scm")))

(define (read-config/guard pathname)
  (guard (c ((i/o-file-does-not-exist-error? c)
             (fatal (cat "specified config file `"
                         (dsp-pathname pathname) "' does not exist."))))
    (call-with-input-file (->namestring pathname)
      read-config)))

(define (parse-options mod cmd-line . options)
  (define common-options
    (list
     (option '("help" #\h) #f #f
             (lambda (opt name val args config)
               (show-help mod)
               (exit 0)))
     (option '("config" #\c) #t #f
             (lambda (opt name val args config)
               (values args val)))
     (option '("no-config") #f #f
             (lambda (opt name val args config)
               (values args #f)))))

  (call-with-values
      (lambda ()
        (args-fold cmd-line (append options common-options)
                   (lambda (opt name arg args config)
                     (display "unrecognized option: " (current-error-port))
                     (display name (current-error-port))
                     (newline (current-error-port))
                     (show-usage mod (current-error-port))
                     (exit 1))
                   (lambda (operand args config)
                     (values (append args (list operand)) config))
                   '() (default-config-location)))
    (lambda (args config)
      (values args
              (if config
                  (read-config/guard config)
                  (default-config))))))

(define* (open-database* config #:key
                         (destination (config-default-name config))
                         (repositories '()))
  (let* ((item (if destination
                   (or (config-ref config destination)
                       (fatal (cat "no such destination configured: " destination)))
                   (config-default-item config)))
         (location (config-item-database-location item)))
    (guard (c ((database-locked-error? c)
               (fatal (cat "database locked: " (dsp-pathname location)))))
      (open-database location
                     (config-item-destination item)
                     (append repositories (config-item-repositories item))
                     (config-item-cache-directory item)))))

(define (call-with-database* config proc)
  (call-with-database (open-database* config)
    (lambda (db)
      (with-throw-handler #t
        (lambda ()
          (proc db))
        (lambda _
          (close-database db))))))
