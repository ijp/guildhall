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
          repository-name
          repository-available-pathname
          repository-fetch-available
          repository-fetch-bundle

          null-repository
          make-file-repository)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :13) string-map)
          (srfi :14 char-sets)
          (spells operations)
          (spells ports)
          (spells pathname)
          (spells filesys)
          (dorodango private utils))

(define-record-type repository
  (fields name ops))

(define-operation (repository/available-pathname repo cache-directory))
(define-operation (repository/fetch-available repo cache-directory))
(define-operation (repository/fetch-bundle repo location cache-directory))

(define (repository-available-pathname repo cache-directory)
  (repository/available-pathname (repository-ops repo) cache-directory))

(define (repository-fetch-available repo cache-directory)
  (repository/fetch-available (repository-ops repo) cache-directory))

(define (repository-fetch-bundle repo location cache-directory)
  (repository/fetch-bundle (repository-ops repo) location cache-directory))

(define null-repository
  (make-repository
   #f
   (object #f
     ((repository/fetch-bundle repo location cache-directory)
      location))))

(define (make-file-repository name directory)
  (let* ((directory (pathname-as-directory directory))
         (available-pathname (pathname-with-file directory "available.scm")))
    (make-repository
     name
     (object #f
       ((repository/available-pathname repo cache-directory)
        cache-directory                 ;ignored
        available-pathname)
       ((repository/fetch-available repo cache-directory)
        cache-directory                 ;ignored
        available-pathname)
       ((repository/fetch-bundle repo location cache-directory)
        (pathname-join directory (location->pathname location)))))))

)

;; Local Variables:
;; scheme-indent-styles: ((object 1))
;; End:
