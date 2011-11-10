;;; destination.scm --- 

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

;; Currently, FHS destinations install a wrapper shell script for each
;; R6RS program. This wrapper executes a destination-specific
;; `r6rs-script', which in turn sets the implementation-specific
;; library search path based on the destination. Once SRFI 103 (and
;; hence R6RS_LIBRARY_PATH) is widely implemented, r6rs-script may be
;; changed to a symlink or dropped in favor of `scheme-script', as
;; recommended by R6RS Appendix D.2. One level of indirection seems
;; inevitable to facilitate setting the library search path
;; environment variable appropriatly for the destination.

;;; Code:
#!r6rs

(library (guildhall destination)
  (export make-destination
          destination?
          destination-name
          destination-prefix
          destination-hooks
          destination-pathname
          destination-open-file
          destination-install
          destination-without-categories
          
          call-with-destination-support-bundle
          package:destination-support
          destination-support-pathname
          
          make-destination-handler)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :13)
                string-concatenate
                string-concatenate-reverse
                string-contains) 
          (only (guile) assq-ref)
          (guildhall spells record-types)
          (guildhall spells pathname)
          (guildhall inventory)
          (guildhall package))

(define-record-type* destination
  (make-destination name prefix call/support-bundle categories hooks)
  ())

(define-functional-fields destination
  name prefix call/support-bundle categories hooks)

(define (destination-without-categories destination categories)
  (destination-modify-categories
   destination
   (lambda (categories)
     (remp (lambda (entry)
             (memq (car entry) categories))
           categories))))

(define (invoke-destination-handler destination category getter . args)
  (cond ((assq-ref (destination-categories destination) category)
         => (lambda (handler)
              (apply (getter handler) args)))
        (else
         (error 'invoke-destination-handler "no handler for category"
                category))))

(define (destination-pathname destination package category pathname)
  (invoke-destination-handler destination
                              category
                              destination-handler-mapper
                              package
                              pathname))

(define (call-with-destination-support-bundle destination options receiver)
  ((destination-call/support-bundle destination) destination options receiver))

(define (destination-open-file destination package category pathname)
  (invoke-destination-handler destination
                              category
                              destination-handler-opener
                              package
                              pathname))

(define (destination-install destination package category pathname extractor)
  (call-with-port (destination-open-file destination package category pathname)
    extractor))

(define-record-type destination-handler
  (fields mapper opener))

(define package:destination-support
  (make-package 'dorodango-support '((0))))

(define (destination-support-pathname destination category pathname)
  (destination-pathname destination
                        package:destination-support
                        category
                        pathname))

)
