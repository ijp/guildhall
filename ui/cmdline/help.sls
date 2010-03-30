;;; help.sls --- commandline help rendering

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

(library (dorodango ui cmdline help)
  (export dsp-help
          dsp-version
          dsp-listing)
  (import (rnrs)
          (spells args-fold)
          (spells fmt)
          (dorodango private utils)
          (dorodango ui cmdline base))

(define (dsp-option-name name)
  (cat (if (string? name) "--" "-") name))

(define (dsp-option/left-side option)
  (cat (fmt-join dsp-option-name (option-names option) ", ")
       (cond ((option-argument option)
              => (lambda (metavar)
                   (cat " " (string-upcase (symbol->string metavar)))))
             (else
              ""))))

(define (dsp-help command)
  (let ((synopsis (command-synopsis command))
        (description (command-description command)))
    (cat "Usage: doro " (car synopsis) "\n"
         (fmt-join/suffix dsp (cdr synopsis) "\n")
         (fmt-indented "  " (car description))
         (fmt-join dsp (cdr description) "\n")
         "\n"
         "Options:\n"
         (dsp-listing "  " (append
                            (map (lambda (option)
                                   (dsp-option/left-side option))
                                 (command-options command))
                            '("--help"))
                      "  " (append (map option-description (command-options command))
                                   '("show this help and exit")))
         "\n"
         (apply-cat (command-footer command)))))

(define (dsp-version)
  (cat "doro 0.0.0\n"
       "Copyright (C) Andreas Rottmann\n"
       (wrap-lines
        "This is free software; see the source for copying conditions."
        "There is NO warranty; not even for MERCHANTABILITY or FITNESS "
        "FOR A PARTICULAR PURPOSE.")
       "\n"))

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
