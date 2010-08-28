;;; base.sls --- command-line UI base library

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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
#!r6rs

(library (dorodango ui cmdline base)
  (export make-cmdline-ui
          define-option

          command?
          command-name
          command-description
          command-synopsis
          command-footer
          command-options
          command-handler
          
          define-command
          find-command
          %commands

          arg-pusher
          arg-setter
          value-setter)
  (import (rnrs)
          (only (srfi :13)
                string-null?
                string-trim-both)
          (spells define-values)
          (spells operations)
          (spells alist)
          (spells args-fold)
          (spells cells)
          (only (spells misc) and=> unspecific)
          (spells record-types)
          (wak foof-loop)
          (wak fmt)
          (dorodango private utils)
          (dorodango ui))


;;; UI helpers

(define (fmt/stdout . formats)
  (fmt #t (apply-cat formats))
  (flush-output-port (current-output-port)))

(define (cmdline/message . formats)
  (fmt/stdout (cat (apply-cat formats) nl)))

(define (cmdline/y-or-n default message)
  (loop prompt-again ()
    (fmt/stdout (cat message " [" (if (eqv? default #t) "Y/n" "y/N") "] "))
    (let ((input (string-downcase (string-trim-both
                                   (get-line (current-input-port))))))
      (if (string-null? input)
          default
          (case (string-ref input 0)
            ((#\y) #t)
            ((#\n) #f)
            (else
             (fmt/stdout (cat "Please answer 'y' or 'n'.\n"))
             (prompt-again)))))))

(define (cmdline/prompt message choices)
  (define (lookup-key keys input)
    (find (lambda (key)
            (cond ((char? key)
                   (char=? key (string-ref input 0)))
                  ((string? key)
                   (string=? key input))
                  (else
                   (assertion-violation 'prompt "Invalid key type " key))))
          keys))
  (loop ((for choice (in-list choices))
         (let-values (key help)
           (values (car choice) (cadr choice)))
         (for keys (listing key))
         (for help-texts (listing (cat key ": " help))))
    => (loop prompt-again ()
         (fmt/stdout (cat message " ["
                      (char-upcase (car keys)) "/" (fmt-join dsp (cdr keys) "/")
                      "/?] "))
         (let ((input (string-downcase (string-trim-both
                                        (get-line (current-input-port))))))
           (cond ((string-null? input)
                  (car keys))
                 ((string=? "?" input)
                  (fmt/stdout
                   (cat "The following choices are available:\n"
                        (fmt-join/suffix (lambda (text) (cat "  " text))
                                         help-texts
                                         "\n")))
                  (prompt-again))
                 ((lookup-key keys input) => values)
                 (else
                  (fmt/stdout
                   (cat "Invalid response. Please enter a valid choice"
                        " or '?' for help.\n"))
                  (prompt-again)))))))

(define (make-cmdline-ui options)
  (let-assq options (assume-yes?)
    (object #f
      ((ui/message ui . formats)
       (apply cmdline/message formats))
      ((ui/y-or-n ui default message . maybe-choose-yes)
       (if assume-yes?
           (if (pair? maybe-choose-yes)
               ((car maybe-choose-yes))
               #t)
           (cmdline/y-or-n default message)))
      ((ui/prompt ui message choices . maybe-choose-yes)
       (let ((choose-yes (or (and (pair? maybe-choose-yes)
                                  (car maybe-choose-yes))
                             (lambda ()
                               (or (and=> (assv #\y choices) car)
                                   (assertion-violation 'ui/prompt
                                    "cannot assume `yes' for these choices"
                                    choices))))))
         (if assume-yes?
             (choose-yes)
             (cmdline/prompt message choices)))))))


;;; Commands

(define %commands (make-cell '()))

(define-record-type* command
  (make-command name description synopsis footer options handler)
  ())

(define (clause-alist->command name clauses)
  (define (list-clause name)
    (cond ((assq name clauses) => cdr)
          (else                   '())))
  (make-command name
                (list-clause 'description)
                (list-clause 'synopsis)
                (list-clause 'footer)
                (list-clause 'options)
                (cond ((assq 'handler clauses)
                       => cadr)
                      (else
                       (assertion-violation 'clause-alist->command
                                            "handler clause missing")))))

(define (find-command name list)
  (find (lambda (command)
          (eq? name (command-name command)))
        list))

(define-syntax define-command
  (syntax-rules (description synopsis options handler)
    ((_ name (clause-name clause-content ...) ...)
     (define-values ()
       (add-command (clause-alist->command
                     'name
                     (list (list 'clause-name clause-content ...)
                           ...)))))))

(define (add-command command)
  (cell-set! %commands (cons command (cell-ref %commands))))

(define (arg-pusher name)
  (lambda (option option-name arg vals)
    (apush name arg vals)))

(define arg-setter
  (case-lambda
    ((name convert)
     (lambda (option option-name arg vals)
       (acons name (convert arg) vals)))
    ((name)
     (arg-setter name values))))

(define (value-setter name value)
  (lambda (option option-name arg vals)
    (acons name value vals)))

(define-syntax define-option
  (syntax-rules ()
    ((_ identifier names argument description processor)
     (define identifier
       (option 'names
               'argument
               #f
               #f
               description
               processor)))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (object 1))
;; End:
