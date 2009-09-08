;;; destination.sls --- 

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

(library (dorodango destination)
  (export destination?
          destination-name
          destination-pathname
          destination-without-categories
          
          make-fhs-destination)
  (import (rnrs)
          (only (srfi :1) split-at)
          (srfi :8 receive)
          (only (srfi :13) string-concatenate)
          (spells alist)
          (spells pathname)
          (dorodango package))

(define-record-type destination
  (fields name categories))

(define (make-fhs-destination name prefix)
  (let ((prefix (pathname-as-directory prefix)))
    (define (prefixer entry)
      (let ((template (cdr entry)))
        (cons (car entry)
              (lambda (package pathname)
                (pathname-join
                 (pathname-as-directory
                  (vals->pathname prefix
                                  `((name . ,(symbol->string (package-name package))))
                                  template))
                 pathname)))))
    (make-destination
     name
     (map prefixer
          '((libraries .  ("share" "r6rs-libs"))
            (documentation . ("share" "doc" ("libr6rs-" name)))
            (programs . ("bin")))))))

(define (destination-without-categories destination categories)
  (make-destination
   (destination-name destination)
   (remp (lambda (entry)
           (memq (car entry) categories))
         (destination-categories destination))))

(define (destination-pathname destination package category pathname)
  (cond ((assq-ref (destination-categories destination) category)
         => (lambda (mapper)
              (mapper package pathname)))
        (else
         #f)))

(define (resolve-template template vals)
  (cond ((symbol? template)
         (assq-ref vals template))
        ((pair? template)
         (string-concatenate (map (lambda (part) (resolve-template part vals)) template)))
        (else
         template)))

(define (vals->pathname base vals template)
  (receive (dir-parts file-part)
           (split-at (map (lambda (part) (resolve-template part vals)) template)
                     (- (length template) 1))
    (make-pathname (pathname-origin base)
                   (append (pathname-directory base) dir-parts)
                   (if (pair? (car file-part))
                       (string-concatenate (car file-part))
                       (car file-part)))))

)
