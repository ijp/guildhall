;;; repository.scm --- Dorodango repositories

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

;;; Code:
#!r6rs

(library (dorodango repository)
  (export repository?
          repository-name
          repository-location
          repository-available-pathname
          repository-fetch-available
          repository-fetch-bundle

          null-repository
          make-file-repository
          make-http-repository
          uri-string->repository)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :13) string-map string-prefix?)
          (srfi :14 char-sets)
          (only (spells misc) or-map)
          (only (spells opt-args) :optional)
          (spells operations)
          (spells ports)
          (spells pathname)
          (spells filesys)
          (spells logging)
          (wak fmt)
          (web uri)
          (web client)
          (web response)
          (dorodango private utils)
          (dorodango ui))

(define-record-type repository
  (fields name ops))

(define-operation (repository/location repo))
(define-operation (repository/available-pathname repo cache-directory))
(define-operation (repository/fetch-available repo cache-directory))
(define-operation (repository/fetch-bundle repo location cache-directory))

(define (repository-location repo)
  (repository/location (repository-ops repo)))

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
     ((repository/location repo)
      "null:")
     ((repository/fetch-bundle repo location cache-directory)
      location))))

(define (make-file-repository name directory)
  (let* ((directory (pathname-as-directory directory))
         (available-pathname (pathname-with-file directory "available.scm")))
    (make-repository
     name
     (object #f
       ((repository/location repo)
        (string-append "file://" (->namestring directory)))
       ((repository/available-pathname repo cache-directory)
        cache-directory                 ;ignored
        available-pathname)
       ((repository/fetch-available repo cache-directory)
        cache-directory                 ;ignored
        (check-existence available-pathname))
       ((repository/fetch-bundle repo location cache-directory)
        (check-existence
         (pathname-join directory (location->pathname location))))))))


;;; HTTP support

(define (relative-uri path rel)
  (build-uri (uri-scheme rel)
             #:userinfo (uri-host rel)
             #:host (uri-host rel)
             #:port (uri-host rel)
             #:path (encode-and-join-uri-path
                     (append (split-and-decode-uri-path (uri-path rel))
                             (split-and-decode-uri-path path)))))

(define (make-http-repository name uri-string)
  (let* ((base-uri (uri-with-directory-path
                    (or (string->uri uri-string)
                        (error "bad URI string" uri-string))))
         (available-uri (relative-uri (string->uri "available.scm") base-uri))
         (available-filename "available.scm"))
    (make-repository
     name
     (object #f
       ((repository/location repo)
        (uri->string base-uri))
       ((repository/available-pathname repo cache-directory)
        (pathname-with-file cache-directory available-filename))
       ((repository/fetch-available repo cache-directory)
        (http-download (repository/available-pathname repo cache-directory)
                       available-uri))
       ((repository/fetch-bundle repo location cache-directory)
        (let ((destination (pathname-join cache-directory
                                          (location->pathname location))))
          (if (file-exists? destination)
              destination
              (http-download destination
                             (relative-uri location base-uri)))))))))

(define (http-download destination uri)
  (message "Fetching " (uri->string uri))
  (call-with-values (lambda ()
                      (http-get uri #:decode-body? #f))
    (lambda (response body)
      (case (response-code response)
        ((200)
         (call-with-output-file/atomic destination 'block
                                       (lambda (port)
                                         (put-bytevector port body)
                                         destination)))
        ;;++ handle redirects
        (else
         (log/repo 'warning
                   (cat "unable to download `" (uri->string uri) "': "
                        (response-code response) " "
                        (response-reason-phrase response)))
         #f)))))


;;; Repository-creating dispatcher

(define supported-repository-types
  (list
   (lambda (name uri-string)
     (and (string-prefix? "file://" uri-string)
          (make-file-repository
           name
           (substring uri-string 7 (string-length uri-string)))))
   (lambda (name uri-string)
     (and (string-prefix? "http://" uri-string)
          (make-http-repository name uri-string)))))

(define (uri-string->repository uri-string . name)
  (or-map (lambda (constructor)
            (constructor (:optional name #f) uri-string))
          supported-repository-types))


;;; Utilities

(define (check-existence pathname)
  (cond ((file-exists? pathname)
         pathname)
        (else
         (log/repo 'warning (cat "repository file `" (dsp-pathname pathname)
                                 "' does not exist"))
         #f)))

(define logger:dorodango.repo (make-logger logger:dorodango 'repo))
(define log/repo (make-fmt-log logger:dorodango.repo))

)

;; Local Variables:
;; scheme-indent-styles: ((object 1) foof-loop)
;; End:
