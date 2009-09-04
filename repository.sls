;;; repository.sls --- Dorodango repositories

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

(library (dorodango repository)
  (export repository?
          repository-available-pathname
          repository-fetch-available
          repository-fetch-bundle

          null-repository)
  (import (rnrs)
          (only (srfi :13) string-map)
          (spells operations))

(define-record-type repository
  (fields ops))

(define-operation (repository/available-pathname repo))
(define-operation (repository/fetch-available repo))
(define-operation (repository/fetch-bundle repo location))

(define (repository-available-pathname repo)
  (repository/available-pathname (repository-ops repo)))

(define (repository-fetch-available repo)
  (repository/fetch-available (repository-ops repo)))

(define (repository-fetch-bundle repo location)
  (repository/fetch-bundle (repository-ops repo) location))

(define null-repository
  (make-repository (object #f
                     ((repository/fetch-bundle repo location)
                      location))))

#;
(define (repository->filename repository filename)
  (string-append
   (string-map (lambda (c)
                 (case c
                   ((#\/) #\_)
                   (else  c)))
               (repository-identifier repository))
   "_" filename))

)

;; Local Variables:
;; scheme-indent-styles: ((object 1))
;; End:
