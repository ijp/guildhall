;;; universe.sls --- Dependency solver, universe public interface

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

;; This library presents a read-only view onto the universe. Here R6RS
;; libraries fall short -- this and the `(dorodango solver internals)'
;; exported interfaces could be less redundantly described using
;; something like `compound-interface' from the Scheme 48 module
;; system.

;; TODO:
;; - Document universe requirements

;;; Code:
#!r6rs

(library (dorodango solver universe)
  (export universe?
          universe-package-count
          universe-version-count
          universe-package-stream
          universe-dependency-stream

          guarantee-universe
          
          package?
          package-id
          package-name
          package-versions
          package-current-version
          
          package=?
          package<?
          package-compare
          package-hash
          package-wt-type

          guarantee-package
          
          version?
          version-id
          version-tag
          version-package
          version-dependencies
          version-reverse-dependencies

          version=?
          version<?
          version-compare
          version-hash
          version-wt-type

          guarantee-version
          
          dependency?
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
  (import (dorodango solver internals))

)
