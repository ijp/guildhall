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
  #:use-module (guild spells logging)
  #:use-module (guild private utils)
  #:use-module (guild database)
  #:use-module (guild package)
  #:use-module (guild config)
  #:use-module (guild repository)
  #:use-module (guild ui formatters)
  #:use-module (srfi srfi-37)
  #:export (show-usage
            make-option
            make-option/arg
            call-with-parsed-options
            call-with-parsed-options+db
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

(define (cmdline-log-handler entry)
  (let ((level-name (log-entry-level-name entry))
        (default-level? (eq? (log-entry-level-name entry) 'info))
        (object (log-entry-object entry)))
    (define (show port)
      (if (procedure? object)
          (object port)
          (display object port))
      (newline port)
      (flush-output-port port))
    (cond
     (default-level?
      (show (current-output-port)))
     (else
      (display level-name (current-error-port))
      (display ": " (current-error-port))
      (show (current-error-port))))))

(define* (call-with-parsed-options mod cmd-line options proc
                                   #:key (config-options? #t))
  (define help-options
    (list
     (make-option '("help" #\h)
                  (lambda ()
                    (show-help mod)
                    (exit 0)))))
  
  (define config (default-config-location))
  (define prefix #f)
  (define config-options
    (list
     (make-option/arg '("config" #\c)
                      (lambda (val) (set! config val)))
     (make-option '("no-config")
                  (lambda () (set! config #f)))
     (make-option/arg '("prefix")
                      (lambda (arg)
                        (set! prefix arg)))))
  
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
    (let-logger-properties ((logger:dorodango
                             `((handlers (,log-level ,cmdline-log-handler)))))
      (proc (reverse args)
            (let ((config (if config
                              (read-config/guard config)
                              (default-config))))
              (if prefix
                  (make-prefix-config
                   prefix
                   (config-item-repositories (config-default-item config)))
                  config))))))

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

(define* (call-with-parsed-options+db mod cmd-line options proc)
  (define dest #f)
  (define repos '())
  (call-with-parsed-options mod cmd-line
                            (append
                             (list
                              (make-option/arg
                               '("dest" #\d)
                               (lambda (arg)
                                 (set! dest (string->symbol arg))))
                              (make-option/arg
                               '("repo" #\r)
                               (lambda (arg)
                                 (set! repos
                                       (append
                                        repos
                                        (list (uri-string->repository arg)))))))
                             options)
    (lambda (args config)
      (call-with-database (open-database* config #:destination dest
                                          #:repositories repos)
        (lambda (db)
          (with-throw-handler #t
            (lambda ()
              (proc args config db))
            (lambda _
              (close-database db))))))))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database* 1) (call-with-database 1) (call-with-parsed-options 3) (call-with-parsed-options+db 3))
;; End:
