;;; formatters.sls --- formatting combinators

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (dorodango ui formatters)
  (export dsp-solution
          dsp-bundle
          dsp-inventory
          dsp-package
          dsp-package-version
          dsp-package-identifier)
  (import (rnrs)
          (only (srfi :13) string-null? string-prefix?)
          (wak fmt)
          (wak foof-loop)
          (spells match)
          (dorodango private utils)
          (dorodango inventory)
          (dorodango package)
          (dorodango bundle)
          (dorodango solver)
          (prefix (dorodango solver universe)
                  universe-)
          (dorodango solver choice)
          (dorodango database dependencies))

(define (dsp-solution solution)
  (let ((choices (list-sort (lambda (c1 c2)
                              (< (choice-id c1) (choice-id c2)))
                            (choice-set->list (solution-choices solution)))))
    (fmt-join/suffix (lambda (choice)
                       (cat (dsp-dependency (choice-dep choice))
                            "\n -> " (dsp-choice choice)))
                     choices
                     "\n")))

(define (dsp-choice choice)
  (let* ((version (choice-version choice))
         (name (universe-package-name (universe-version-package version))))
    (cond ((universe-version-tag version)
           => (lambda (tag)
                (cat "Installing " name " " (dsp-package-version tag))))
          (else
           (cat "Removing " name)))))

(define (dsp-package-version version)
  (fmt-join (lambda (part)
              (fmt-join dsp part "."))
            version
            "-"))

(define (dsp-package-identifier package)
  (cat (package-name package)
       " (" (dsp-package-version (package-version package)) ")"))

(define (dsp-dependency dependency)
  (let ((info (universe-dependency-tag dependency)))
    (cat (package->string (dependency-info-package info) " ")
         " depends upon "
         (fmt-join dsp-dependency-choice (dependency-info-choices info) " or "))))

(define (dsp-dependency-choice choice)
  (let ((constraint (dependency-choice-version-constraint choice)))
    (cat (dependency-choice-target choice)
         (if (null-version-constraint? constraint)
             fmt-null
             (cat " " (wrt/unshared (version-constraint->form constraint)))))))

(define (dsp-package pkg . extra-fields)
  (cat "Package: " (package-name pkg) "\n"
       (apply-cat extra-fields)
       (cat "Version: " (dsp-package-version (package-version pkg)) "\n")
       (let ((depends (package-property pkg 'depends '())))
         (if (null? depends)
             fmt-null
             (cat "Depends: " (fmt-join wrt depends ", ") "\n")))
       (fmt-join (lambda (category)
                   (let ((inventory (package-category-inventory pkg category)))
                     (if (inventory-empty? inventory)
                         fmt-null
                         (cat "Category: " category "\n"
                              (dsp-inventory inventory)))))
                 (package-categories pkg))
       (cond ((package-homepage pkg)
              => (lambda (homepage)
                   (cat "Homepage: " homepage "\n")))
             (else
              fmt-null))
       "Description: " (package-synopsis pkg)
       "\n"
       (fmt-indented " " (dsp-package-description pkg))))

(define (dsp-package-description pkg)
  (define (flush-text text blocks)
    (if (null? text)
        blocks
        (cons (wrap-lines (fmt-join dsp (reverse text) " ")) blocks)))
  (loop continue ((for line (in-list (package-description pkg)))
                  (with text '())
                  (with blocks '()))
    => (apply-cat (reverse (flush-text text blocks)))
    (if (or (string-null? line) (string-prefix? " " line))
        (continue (=> blocks
                      (cons (cat line "\n") (flush-text text blocks)))
                  (=> text '()))
        (continue (=> text (cons line text))))))

(define (dsp-inventory inventory)
  (define (dsp-node node path)
    (lambda (state)
      (loop next ((for cursor (in-inventory node))
                  (with st state))
        => st
        (let ((path (cons (inventory-name cursor) path)))
          (if (inventory-leaf? cursor)
              (next (=> st ((cat " " (fmt-join dsp (reverse path) "/") "\n")
                            st)))
              (next (=> st ((dsp-node cursor path) st))))))))
  (dsp-node inventory '()))

(define (dsp-bundle bundle)
  (fmt-join dsp-package (bundle-packages bundle) "\n"))

)

;; Local Variables:
;; scheme-indent-styles: ((cases 2) foof-loop)
;; End:
