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
(define-module (guild cli ui)
  #:use-module (srfi srfi-39)
  #:use-module (rnrs io ports)
  #:use-module ((rnrs lists) #:select (find))
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module (guild ext fmt)
  #:use-module (guild ext foof-loop)
  #:use-module (guild spells operations)
  #:use-module (guild ui)
  #:use-module (guild cli)
  #:use-module (guild cli db)
  #:export (call-with-parsed-options/config+db/ui))

(define (fmt/stdout . formats)
  (fmt #t (apply-cat formats))
  (force-output))

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

(define (make-cmdline-ui assume-yes? non-interactive?)
  (object #f
    ((ui/message ui . formats)
     (apply cmdline/message formats))
    ((ui/y-or-n ui default message . maybe-choose-noninteractively)
     (cond (assume-yes?
            #t)
           ((and non-interactive? (pair? maybe-choose-noninteractively))
            ((car maybe-choose-noninteractively)))
           (non-interactive?
            default)
           (else
            (cmdline/y-or-n default message))))
    ((ui/prompt ui message choices . maybe-choose-noninteractively)
     (define (choose-yes)
       (or (and=> (assv #\y choices) car)
           (assertion-violation 'ui/prompt
                                "cannot assume `yes' for these choices"
                                choices)))
     (let ((choose-noninteractively
            (if (pair? maybe-choose-noninteractively)
                (car maybe-choose-noninteractively)
                choose-yes)))
       (cond (assume-yes?      (choose-yes))
             (non-interactive? (choose-noninteractively))
             (else             (cmdline/prompt message choices)))))))

(define (call-with-parsed-options/config+db/ui mod cmd-line options proc)
  (define assume-yes? #f)
  (define non-interactive? #f)
  (call-with-parsed-options/config+db
      mod cmd-line
      (append (list
               (make-option
                '("yes" #\y)
                (lambda () (set! assume-yes? #t)))
               (make-option
                '("non-interactive" #\n)
                (lambda () (set! non-interactive? #t))))
              options)
    (lambda (args config db)
      (parameterize
          ((current-ui (make-cmdline-ui assume-yes? non-interactive?)))
        (proc args config db)))))

;; Local Variables:
;; scheme-indent-styles: ((call-with-parsed-options/config+db/ui 3))
;; End:
