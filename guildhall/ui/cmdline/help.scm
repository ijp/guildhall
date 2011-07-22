;;; help.scm --- commandline help rendering

;; Copyright (C) 2011 Free Software Foundation, Inc.
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

(library (guildhall ui cmdline help)
  (export dsp-help
          dsp-man-page
          indented-help-formatter
          dsp-version
          dsp-command-listing)
  (import (rnrs)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (system base lalr)
          (only (srfi :13) string-fold-right string-map)
          (spells operations)
          (spells args-fold)
          (guildhall ext fmt)
          (only (guile) assq-ref)
          (ice-9 match)
          (guildhall private utils)
          (guildhall ui cmdline base))

(define-operation (help/dsp-section formatter heading . body))
(define-operation (help/dsp-synopsis formatter synopsis))
(define-operation (help/dsp-description formatter description))
(define-operation (help/dsp-option-listing formatter options))
(define-operation (help/dsp-command-listing formatter commands))

(define-syntax define-wrappers
  (syntax-rules ()
    ((define-wrappers (name formatter) ...)
     (begin
       (define name (make-help-formatter-wrapper formatter))
       ...))))

(define (make-help-formatter-wrapper operation)
  (lambda args
    (lambda (st)
      ((apply operation (help-formatter st) args) st))))

(define-wrappers
  (dsp-section help/dsp-section)
  (dsp-command-listing help/dsp-command-listing)
  (dsp-synopsis help/dsp-synopsis)
  (dsp-description help/dsp-description)
  (dsp-option-listing help/dsp-option-listing))

(define (help-formatter st)
  (fmt-ref st 'help-formatter))

(define (help-level st)
  (fmt-ref st 'help-level))

(define (dsp-help formatter command)
  (fmt-let 'help-formatter formatter
    (dsp-synopsis (command-synopsis command))
    "\n"
    (dsp-description (command-description command))
    "\n"
    (dsp-option-listing (command-options command))
    (if (null? (command-footer command))
        fmt-null
        (cat "\n" (apply-cat (command-footer command))))))

(define (dsp-version)
  (cat "doro 0.0.0\n"
       (dsp-copyright)))

(define (dsp-copyright)
  (cat
   "Copyright (C) 2009-2010 Andreas Rottmann\n"
   "\n"
   "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
   "This is free software: you are free to change and redistribute it.\n"
   "There is NO WARRANTY, to the extent permitted by law.\n"))

(define (dsp-option-name name)
  (cat (if (string? name) "--" "-") name))


;;; Indented formatter

(define (indented/dsp-synopsis prefix0 prefix synopsis)
  (let ((sep (string-append "\n"
                            (make-string (string-length prefix0) #\space)
                            prefix)))
    (cat prefix0 prefix (fmt-join dsp synopsis sep)
         (if (null? (cdr synopsis)) fmt-null "\n"))))

(define (indented/dsp-option formatter option)
  (cat (fmt-join dsp-option-name (option-names option) ", ")
       (cond ((option-argument option)
              => (lambda (metavar)
                   (cat " " (string-upcase (symbol->string metavar)))))
             (else
              ""))))

(define indented-help-formatter
  (object #f
    ((help/dsp-section self heading . body)
     (cat heading ":\n" (apply fmt-indented "  " body)))
    ((help/dsp-synopsis self synopsis)
     (indented/dsp-synopsis "Usage: " "doro " synopsis))
    ((help/dsp-description self description)
     (cat (fmt-indented "  " (car description))
          (fmt-join dsp (cdr description) "\n")))
    ((help/dsp-option-listing self options)
     (if (null? options)
         fmt-null
         (help/dsp-section self "Options"
           (dsp-listing "" (map (lambda (option)
                                  (indented/dsp-option self option))
                                options)
                        "  " (map option-description options)))))
    ((help/dsp-command-listing self commands)
     (dsp-listing "  " (map command-name commands)
                  "  " (map (lambda (command)
                              (apply-cat (command-description command)))
                            commands)))))


;;; Manpage formatter

(define man-help-formatter
  (object #f
    ((help/dsp-section self heading . body)
     (lambda (st)
       (let ((level (help-level st)))
         ((fmt-let 'help-level (+ level 1)
            (cat (if (= level 0)
                     (cat ".Sh " (string-upcase heading) "\n")
                     fmt-null)
                 (apply-cat body)))
          st))))
    ((help/dsp-synopsis self synopsis)
     (help/dsp-section self "Synopsis"
       (man/dsp-synopsis "doro" synopsis)))
    ((help/dsp-description self description)
     (help/dsp-section self "Description"
       (cat (dsp-man (car description))
            (if (null? (cdr description))
                fmt-null
                (cat ".Pp\n"
                     (fmt-join dsp-man (cdr description) "\n"))))))
    ((help/dsp-option-listing self options)
     (if (null? options)
         fmt-null
         (help/dsp-section self "Options"
           ".Bl -tag -width flag\n"
           (fmt-join man/dsp-option options)
           ".El\n")))
    ((help/dsp-command-listing self commands)
     (dsp-listing "  " (map command-name commands)
                  "  " (map (lambda (command)
                              (apply-cat (command-description command)))
                            commands)))))

(define (dsp-man format)
  (lambda (st)
    (let ((s (fmt-start #f (format-state-modifier st) (dsp format))))
      ((fmt-join dsp (string-fold-right
                      (lambda (c seed)
                        (cons (case c
                                ((#\-) "\\-")
                                (else  (string c)))
                              seed))
                      '()
                      s))
       st))))

(define (format-state-modifier st)
  (lambda (new-st)
    (if (port? new-st)
        (fmt-set-port! (copy-fmt-state st) new-st)
        new-st)))

(define (man/dsp-option option)
  (define (dsp-option-name name)
    (cond ((string? name) (cat "Fl \\-" (dsp-man name) " "))
          (else           (cat "Fl " (string name) " "))))
  (cat ".It "
       (fmt-join dsp-option-name (option-names option) ", ")
       (cond ((option-argument option)
              => (lambda (metavar)
                   (cat "Ar " (dsp-man (string-upcase (symbol->string metavar))))))
             (else
              ""))
       "\n"
       (dsp-man (option-description option)) "\n"))

(define (man/dsp-synopsis name synopsis)
  (lambda (st)
    (let ((level (help-level st)))
      (define (dsp-macro name . args)
        (if (<= level 1)
            (cat "." name " " (fmt-join dsp args " ") "\n")
            (cat name " " (fmt-join/suffix dsp args " "))))
      (define (dsp-token token)
        (match token
          (('optional tokens)
           (cat (dsp-macro "Oo")
                (fmt-join dsp-token tokens)
                (dsp-macro "Oc")))
          ((? string? string)
           (dsp-macro "Nm" string))
          (('meta-var string)
           (dsp-macro "Ar" string))
          ('ellipsis
           (dsp "... "))))
      (define (dsp-synopsis-line line)
        (cat (if (<= level 1)
                 (cat ".Nm " name "\n")
                 ".It ")
             (fmt-join dsp-token (parse-synopsis line))))
      ((cat (fmt-join dsp-synopsis-line synopsis "\n")) st))))

(define (dsp-man-page command-list)
  (let ((formatter man-help-formatter))
    (fmt-let 'help-formatter formatter
      (fmt-let 'help-level 0
        ".Dd March 30, 2010\n"
        ".Dt DORO 1\n"
        ".Sh NAME\n"
        ".Nm doro\n"
        ".Nd An R6RS package manager\n"
        (let ((main (find-command 'main command-list)))
          (cat (dsp-synopsis (command-synopsis main))
               (dsp-description (command-description main))))
        (dsp-section "Commands"
          ".Bl -tag -width \"  \"\n"
          (fmt-join (lambda (command)
                      (dsp-help formatter command))
                    (filter (lambda (command)
                              (not (eq? 'main (command-name command))))
                            command-list))
          ".El\n")
        (dsp-section "See Also"
          "The full documentation for \n.Nm doro\nis maintained as a Texinfo "
          "manual. If the info and \n.Nm doro\nprograms are properly installed "
          "at your site, the command\n"
          "\n"
          ".D1 Ic info doro\n"
          "\n"
          "should give you access to the complete manual.\n")
        (dsp-section "Copyright"
          (dsp-copyright))))))


;;; Synopsis line parser

(define token-delimiters
  (char-set-complement
   (char-set-union char-set:letter+digit (char-set #\- #\_))))

(define (synopsis-tokenizer str)
  (let ((i 0) (len (string-length str)))
    (define (peek)
      (and (< i len)
           (string-ref str i)))
    (define (advance!)
      (set! i (+ i 1)))
    (lambda ()
      (let lp ()
        (let ((c (peek)))
          (cond
           ((not c) '*eoi*)
           ((char-whitespace? c) (advance!) (lp))
           ((eqv? c #\[)
            (set! i (+ 1 i))
            (make-lexical-token 'left-bracket i #f))
           ((eqv? c #\])
            (set! i (+ 1 i))
            (make-lexical-token 'right-bracket i #f))
           ((string-prefix? "..." str 0 3 i)
            (set! i (+ 3 i))
            (make-lexical-token 'ellipsis i #f))
           (else
            (let* ((end (or (string-index str token-delimiters i) len))
                   (tok (substring str i end)))
              (set! i end)
              (cond
               ((string-null? tok)
                (error "bad char in string" (substring str i)))
               ((string-index tok char-set:upper-case)
                (make-lexical-token 'meta-var i tok))
               (else
                (make-lexical-token 'literal i tok)))))))))))

(define (parse-synopsis synopsis)
  ((lalr-parser
    ;; terminal (i.e. input) token types
    (left-bracket right-bracket ellipsis meta-var literal)

    (Tokens (TokenList *eoi*) : $1)

    (TokenList (Token TokenList) : (cons $1 $2)
               (Token) : (list $1))

    (Token (literal) : $1
           (meta-var) : `(meta-var ,$1)
           (ellipsis) : 'ellipsis
           (Optional) : $1)

    (Optional (left-bracket TokenList right-bracket) : `(optional ,$2)))
   (synopsis-tokenizer synopsis)
   error))



;; This could use a better name
(define (dsp-listing indent left-items separator right-items)
  (lambda (st)
    (let* ((left-sides
            (map (lambda (left)
                   (fmt #f (cat indent left)))
                 left-items))
           (left-width (fold-left max 0 (map string-length left-sides))))
      ((apply-cat
        (map (lambda (left right)
               (columnar left-width (dsp left)
                         separator
                         (with-width (- 78 left-width) (wrap-lines right))))
             left-sides right-items))
       st))))

)


;; Local Variables:
;; scheme-indent-styles: (as-match
;;                        (fmt-let 2)
;;                        (object 1)
;;                        (dsp-section 1)
;;                        (help/dsp-section 2))
;; End:
