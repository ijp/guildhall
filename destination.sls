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
          destination-prefix
          destination-pathnames
          destination-install
          destination-without-categories
          
          make-fhs-destination)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) split-at)
          (srfi :8 receive)
          (only (srfi :13) string-concatenate)
          (spells alist)
          (only (spells misc) and=>)
          (spells lazy)
          (spells fmt)
          (spells pathname)
          (spells filesys)
          (spells logging)
          (spells process) ;just needed for `chmod'
          (spells sysutils) ;ditto
          (dorodango private utils)
          (dorodango package))

(define-record-type destination
  (fields name prefix categories))

(define (destination-without-categories destination categories)
  (make-destination
   (destination-name destination)
   (destination-prefix destination)
   (remp (lambda (entry)
           (memq (car entry) categories))
         (destination-categories destination))))

(define (destination-pathnames destination package category pathname)
  (cond ((assq-ref (destination-categories destination) category)
         => (lambda (handler)
              (handler-map handler package pathname)))
        (else
         '())))

(define (destination-install destination package category pathname extractor)
  (and=> (assq-ref (destination-categories destination) category)
         (lambda (handler)
           (handler-install handler package pathname extractor))))

(define-record-type handler
  (fields mapper installer))

(define (handler-map handler package pathname)
  ((handler-mapper handler) package pathname))

(define (handler-install handler package pathname extractor)
  ((handler-installer handler) package pathname extractor))


;;; FHS destination

(define (make-fhs-destination name prefix)
  (let ((prefix (pathname-as-directory prefix)))
    (make-destination
     name
     prefix
     `((libraries
        . ,(make-simple-handler prefix fhs-libraries-template))
       (documentation
        . ,(make-simple-handler prefix '("share" "doc" ("libr6rs-" name))))
       (programs
        . ,(make-program-handler/sh-wrapper
            prefix
            '("share" ("libr6rs-" name) "programs")
            '("bin")))))))

(define fhs-libraries-template '("share" "r6rs-libs"))

(define (make-simple-handler prefix template)
  (make-handler
   (lambda (package pathname)
     (list (destination-pathname prefix template package pathname)))
   (lambda (package pathname extractor)
     (let ((dest-pathname (destination-pathname prefix template package pathname)))
       (create-directory* (pathname-with-file dest-pathname #f))
       (let ((filename (->namestring dest-pathname)))
         (log/fhs 'debug "installing " filename)
         (call-with-port (open-file-output-port filename)
           extractor))))))

(define (make-program-handler/sh-wrapper prefix
                                         program-template
                                         sh-wrapper-template)
  (make-handler
   (lambda (package pathname)
     (list (destination-pathname prefix sh-wrapper-template package pathname)
           (destination-pathname prefix program-template package pathname)))
   (lambda (package pathname extractor)
     (let ((program-pathname (destination-pathname prefix
                                                   program-template
                                                   package
                                                   pathname))
           (sh-wrapper-pathname (destination-pathname prefix
                                                      sh-wrapper-template
                                                      package
                                                      pathname))
           (chmod-path (or (force %chmod-path)
                           (log/fhs 'warn "`chmod' not found in PATH"))))
       (create-directory* (pathname-with-file program-pathname #f))
       (let ((filename (->namestring program-pathname)))
         (log/fhs 'debug "installing " filename)
         (call-with-port (open-file-output-port filename)
           extractor))
       (create-directory* (pathname-with-file sh-wrapper-pathname #f))
       (let ((filename (->namestring sh-wrapper-pathname)))
         (log/fhs 'debug "creating shell wrapper" filename)
         (call-with-output-file filename
           (lambda (port)
             (fmt port (dsp-sh-wrapper prefix package program-pathname))))
         (when chmod-path
           (run-process #f chmod-path "+x" filename)))))))

(define %chmod-path (delay (find-exec-path "chmod")))

(define (dsp-sh-wrapper prefix package program-pathname)
  (let ((library-path
         (destination-pathname prefix
                               fhs-libraries-template
                               package
                               (make-pathname #f '() #f))))
    (cat "#!/bin/sh\n"
         "# Shell wrapper for package " (package-identifier package) "\n"
         "\n"
         "export R6RS_LIBRARY_PATH=\"" (->namestring library-path) ;++quote
         "${R6RS_LIBRARY_PATH:+:$R6RS_LIBRARY_PATH}\"\n"
         "exec r6rs-script " (->namestring program-pathname) " \"$@\"\n")))

(define (destination-pathname prefix template package pathname)
  (pathname-join
   (pathname-as-directory
    (vals->pathname prefix
                    `((name . ,(symbol->string (package-name package))))
                    template))
   pathname))
  
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

(define logger:dorodango.fhs-destination
  (make-logger logger:dorodango 'fhs-destination))
(define log/fhs (make-fmt-log logger:dorodango.fhs-destination))

)
