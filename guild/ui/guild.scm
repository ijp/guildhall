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
  #:export (show-usage
            make-option
            make-option/arg
            call-with-parsed-options
            call-with-database*))

(define* (show-usage mod #:optional (port (current-output-port)))
  (display "Usage: " port)
  (display (module-ref mod '%synopsis) port)
  (newline port))

(define* (show-help mod #:optional (port (current-output-port)))
  (show-usage mod port)
  (display (module-ref mod '%help) port))

(define (make-option names handler)
  (option names #f #f
          (lambda (opt name val args)
            (handler)
            args)))

(define (make-option/arg names handler)
  (option names #t #f 
          (lambda (opt name val args)
            (handler val)
            args)))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "dorodango") "config.scm")))

(define (read-config/guard pathname)
  (guard (c ((i/o-file-does-not-exist-error? c)
             (fatal (cat "specified config file `"
                         (dsp-pathname pathname) "' does not exist."))))
    (call-with-input-file (->namestring pathname)
      read-config)))

(define* (call-with-parsed-options mod cmd-line options proc
                                   #:key (config-options? #t))
  (define help-options
    (list
     (make-option '("help" #\h)
                  (lambda ()
                    (show-help mod)
                    (exit 0)))))
  
  (define config (default-config-location))

  (define config-options
    (list
     (make-option/arg '("config" #\c)
                      (lambda (val) (set! config val)))
     (make-option '("no-config")
                  (lambda () (set! config #f)))))
  
  (define log-level 'info)

  (define logger-options
    (list
     (make-option/arg '("log-level" #\l)
                      (lambda (arg)
                        (set! log-level (string->symbol arg))))))
  
  (let ((args (args-fold cmd-line
                         (append options
                                 help-options
                                 (if config-options? config-options '())
                                 logger-options)
                         (lambda (opt name arg args)
                           (display "unrecognized option: " (current-error-port))
                           (display name (current-error-port))
                           (newline (current-error-port))
                           (show-usage mod (current-error-port))
                           (exit 1))
                         (lambda (operand args)
                           (cons operand args))
                         '())))
    (proc (reverse args)
          (if config
              (read-config/guard config)
              (default-config)))))

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
