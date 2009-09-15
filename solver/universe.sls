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

;;; Code:
#!r6rs

(library (dorodango solver universe)
  (export universe?
          dsp-universe)
  (import (rnrs)
          (spells fmt)
          (spells foof-loop)
          (spells lazy-streams)
          (dorodango solver internals))

(define (fmt-stream/suffix formatter stream sep)
  (let ((sep (dsp sep)))
    (lambda (st)
      (loop ((for item (in-stream stream))
             (with st st (sep ((formatter item) st))))
        => st))))

(define (dsp-universe universe)
  (cat
   (fmt-stream/suffix dsp-package (universe-package-stream universe) "\n")
   "\n"
   (fmt-stream/suffix dsp-dependency (universe-dependency-stream universe) "\n")))

(define (dsp-package package)
  (cat "package " (package-name package)
       " <" (fmt-join dsp (map version-tag (package-versions package)) " ")
       ">"))

(define (dsp-dependency dependency)
  (cat "dependency " (dsp-version (dependency-source dependency))
       " -> " (fmt-join dsp-version (dependency-targets dependency) " ")))

(define (dsp-version version)
  (cat (package-name (version-package version)) ":" (version-tag version)))

)
