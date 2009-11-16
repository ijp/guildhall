;;; dsp-universe.sls --- formatters related to the solver universe

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (dorodango ui dsp-universe)
  (export dsp-solution
          dsp-package-version)
  (import (rnrs)
          (spells fmt)
          (spells foof-loop)
          (dorodango solver)
          (dorodango solver universe)
          (dorodango solver choice)
          (dorodango database dependencies)
          (prefix (dorodango package) db:))

(define (dsp-solution solution)
  (let ((choices (list-sort (lambda (c1 c2)
                              (< (choice-id c1) (choice-id c2)))
                            (choice-set->list (solution-choices solution)))))
    (fmt-join/suffix (lambda (choice)
                       (cat (dsp-dependency (choice-dep choice))
                            " -> " (dsp-choice choice)))
                     choices
                     "\n")))

(define (dsp-choice choice)
  (let ((version (choice-version choice)))
    (cond ((version-tag version)
           (cat "Installing " (dsp-version version)))
          (else
           (cat "Removing " (package-name (version-package version)))))))

(define (dsp-version version)
  (cat (package-name (version-package version))
       " (" (dsp-package-version (version-tag version)) ")"))

(define (dsp-package-version version)
  (fmt-join (lambda (part)
              (fmt-join dsp part "."))
            version
            "-"))

(define (dsp-dependency dependency)
  (let ((info (dependency-tag dependency)))
    (cat (db:package-identifier (dependency-info-package info))
         " depends upon "
         (fmt-join dsp-dependency-choice (dependency-info-choices info) " or "))))

(define (dsp-dependency-choice choice)
  (let ((constraint (db:dependency-choice-version-constraint choice)))
    (cat (db:dependency-choice-target choice)
         (if (db:null-version-constraint? constraint)
             fmt-null
             (cat " " (wrt/unshared (db:version-constraint->form constraint)))))))

)

;; Local Variables:
;; scheme-indent-styles: ((cases 2))
;; End:
