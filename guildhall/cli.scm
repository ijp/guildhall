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
(define-module (guildhall cli)
  #:use-module (guildhall spells logging)
  #:use-module (guildhall private utils)
  #:autoload (scripts help) (show-help show-usage)
  #:use-module (srfi srfi-37)
  #:export (make-option
            make-option/arg
            call-with-parsed-options))

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

(define (cmdline-log-handler entry)
  (let ((level-name (log-entry-level-name entry))
        (default-level? (eq? (log-entry-level-name entry) 'info))
        (object (log-entry-object entry)))
    (define (show port)
      (if (procedure? object)
          (object port)
          (display object port))
      (newline port)
      (force-output port))
    (cond
     (default-level?
      (show (current-output-port)))
     (else
      (display level-name (current-error-port))
      (display ": " (current-error-port))
      (show (current-error-port))))))

(define* (call-with-parsed-options mod cmd-line options proc)
  (define help-options
    (list
     (make-option '("help" #\h)
                  (lambda ()
                    (show-help mod)
                    (exit 0)))))
  
  (define log-level 'info)
  (define logger-options
    (list
     (make-option/arg '("log-level" #\l)
                      (lambda (arg)
                        (set! log-level
                              (or (string->number arg)
                                  (string->symbol arg)))))))

  (let ((args (args-fold cmd-line
                         (append options
                                 help-options
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
    (let-logger-properties ((logger:dorodango
                             `((handlers (,log-level ,cmdline-log-handler)))))
      (proc (reverse args)))))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1) (call-with-parsed-options 3))
;; End:
