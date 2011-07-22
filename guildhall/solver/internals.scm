;;; internals.scm --- Dependency solver, internal library

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

;; This library implements the data structures of the universe the
;; resolver works in, and exports both accessors and mutators for
;; them. It should not be imported directly, but used through the
;; `(guildhall solver universe)' library, which provides a read-only
;; view onto the universe. The obvious exception is code that needs to
;; _create_ a universe.

;; NB: In contrast to the aptitude implementation, where everything is
;; templatized and "generic", this is rather concrete; all relevant
;; datastructures (packages, versions, dependencies and tiers) are
;; defined here. The only parameterizability is through the
;; `make-universe' procedure.

;;; Code:
#!r6rs

(library (guildhall solver internals)
  (export make-universe
          universe?
          universe-package-count
          universe-version-count
          universe-package-stream
          universe-dependency-stream
          guarantee-universe
          
          make-package
          package?
          package-id
          package-name
          package-versions
          package-current-version
          set-package-versions!
          set-package-current-version!

          package=?
          package<?
          package-compare
          package-hash
          package-wt-type
          guarantee-package
          
          make-version
          make-uninstalled-version
          version?
          version-id
          version-tag
          version-package
          version-dependencies
          
          version=?
          version<?
          version-compare
          version-hash
          version-wt-type
          
          version-reverse-dependencies
          version-add-dependency!
          version-add-reverse-dependency!
          guarantee-version
          
          make-dependency
          dependency?
          dependency-tag
          dependency-source
          dependency-targets

          dependency=?
          dependency<?
          dependency-compare
          dependency-hash
          dependency-wt-type
          guarantee-dependency
          
          make-tier
          tier?
          tier-policy
          tier-priority
          
          tier=?
          tier<?
          tier<=?
          tier>?
          tier>=?
          tier-compare
          tier-wt-type
          guarantee-tier

          minimum-tier
          defer-tier
          already-generated-tier
          conflict-tier
          maximum-tier)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic fixnums)
          (srfi :67 compare-procedures)
          (guildhall ext define-values)
          (spells hash-utils)
          (spells record-types)
          (guildhall ext wt-tree)
          (only (guildhall private utils)
                define-guarantor))


;;; Universe

(define-record-type* universe
  (make-universe package-stream
                 package-count
                 version-count
                 dependency-stream)
  ())

(define-guarantor guarantee-universe universe? "universe")


;;; Packages

(define-record-type* package
  (make-package id name)
  ((versions #f)
   (current-version #f)))

(define (package=? p1 p2)
  (= (package-id p1) (package-id p2)))

(define (package-compare p1 p2)
  (number-compare (package-id p1) (package-id p2)))

(define (package<? p1 p2)
  (< (package-compare p1 p2) 0))

(define (package-hash package)
  (package-id package))

(define package-wt-type (make-wt-tree-type package<?))

(define-guarantor guarantee-package package? "package")


;;; Versions

(define-record-type* version
  (make-version id tag package)
  ((dependencies '())
   (reverse-dependencies '())))

(define (version=? v1 v2)
  (= (version-id v1) (version-id v2)))

(define (version<? v1 v2)
  (< (version-compare v1 v2) 0))

(define (version-compare v1 v2)
  (number-compare (version-id v1) (version-id v2)))

(define (version-hash version)
  (version-id version))

(define version-wt-type (make-wt-tree-type version<?))

(define (version-add-dependency! version dependency)
  (set-version-dependencies!
   version
   (cons (guarantee-dependency dependency 'version-add-dependency!)
         (version-dependencies version))))

(define (version-add-reverse-dependency! version dependency)
  (set-version-reverse-dependencies!
   version
   (cons (guarantee-dependency dependency 'version-add-reverse-dependency!)
         (version-reverse-dependencies version))))

(define (make-uninstalled-version id package)
  (make-version id #f (guarantee-package package 'make-uninstalled-version)))

(define-guarantor guarantee-version version? "version")


;;; Dependencies

(define-record-type* dependency
  (make-dependency id tag source targets)
  ())

(define (dependency=? d1 d2)
  (eq? d1 d2))

(define (dependency<? d1 d2)
  (< (dependency-id d1) (dependency-id d2)))

(define (dependency-compare d1 d2)
  (number-compare (dependency-id d1) (dependency-id d2)))

(define (dependency-hash dependency)
  (dependency-id dependency))

(define dependency-wt-type (make-wt-tree-type dependency<?))

(define-guarantor guarantee-dependency dependency? "dependency")


;;; Tiers

(define-record-type* tier
  (really-make-tier policy priority)
  ())

(define make-tier
  (case-lambda
    ((policy priority)
     (really-make-tier policy priority))
    ((policy)
     (really-make-tier policy (least-fixnum)))))

(define-values (maximum-tier
                conflict-tier
                already-generated-tier
                defer-tier
                minimum-tier)
  (let ((fx-max (greatest-fixnum)))
    (apply values (map make-tier (list fx-max
                                       fx-max
                                       (- fx-max 1)
                                       (- fx-max 2)
                                       (least-fixnum))))))

(define (tier-compare t1 t2)
  (refine-compare
   (number-compare (tier-policy t1) (tier-policy t2))
   (number-compare (tier-priority t1) (tier-priority t2))))

(define (tier=? t1 t2)
  (and (= (tier-policy t1) (tier-policy t2))
       (= (tier-priority t1) (tier-priority t2))))

(define tier<? (<? tier-compare))
(define tier>? (>? tier-compare))
(define tier>=? (>=? tier-compare))
(define tier<=? (<=? tier-compare))

(define tier-wt-type (make-wt-tree-type tier<?))

(define-guarantor guarantee-tier tier? "tier")

)
