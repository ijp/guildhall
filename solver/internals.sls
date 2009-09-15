;;; internals.sls --- Dependency solver, internal library

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

(library (dorodango solver internals)
  (export make-universe
          universe?
          universe-package-count
          universe-version-count
          universe-package-stream
          universe-dependency-stream
          
          make-package
          package?
          package-id
          package-name
          package-versions
          package-current-version
          set-package-versions!
          set-package-current-version!
          
          make-version
          make-uninstalled-version
          version?
          version-id
          version-tag
          version-package
          version-dependencies
          version-reverse-dependencies
          version-add-dependency!
          version-add-reverse-dependency!
          
          make-dependency
          dependency?
          dependency-source
          dependency-targets)
  (import (rnrs base)
          (spells record-types))

(define-record-type* universe
  (make-universe package-stream
                 package-count
                 version-count
                 dependency-stream)
  ())

(define-record-type* package
  (make-package id name)
  ((versions #f)
   (current-version #f)))

(define-record-type* version
  (make-version id tag package)
  ((dependencies '())
   (reverse-dependencies '())))

(define (version-add-dependency! version dependency)
  (set-version-dependencies!
   version
   (cons dependency (version-dependencies version))))

(define (version-add-reverse-dependency! version dependency)
  (set-version-reverse-dependencies!
   version
   (cons dependency (version-reverse-dependencies version))))

(define (make-uninstalled-version id package)
  (make-version id #f package))

(define-record-type* dependency
  (make-dependency source targets)
  ())

)
