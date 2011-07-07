;;; compat.ikarus.sls --- filesys compat library for Ikarus.

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(library (spells filesys compat)
  (export file-exists?
          create-directory
          create-symbolic-link
          create-hard-link
          delete-file
          rename-file
          
          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-stream?
          open-directory-stream
          close-directory-stream
          read-directory-stream
          
          working-directory
          with-working-directory

          library-search-paths)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :13) string-prefix?) ;for exception kludge
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)
          (prefix (ikarus) ik:))

(define ->fn ->namestring)

(define (file-exists? pathname)
  (ik:file-exists? (->fn pathname)))

(define (create-directory pathname)
  (with-i/o-condition-adornment
    (lambda () (ik:make-directory (->fn pathname)))))

(define (delete-file pathname)
  (let ((fname (->fn pathname)))
    (cond ((ik:file-symbolic-link? fname)
           (ik:delete-file fname))
          ((ik:file-directory? fname)
           (ik:delete-directory fname #t))
          ((ik:file-exists? fname)
           (ik:delete-file fname)))))

(define (rename-file source-pathname target-pathname)
  (ik:rename-file (->fn source-pathname) (->fn target-pathname)))

(define (create-hard-link old-pathname new-pathname)
  (with-i/o-condition-adornment
    (lambda () (ik:make-hard-link (->fn old-pathname) (->fn new-pathname)))))

(define (create-symbolic-link old-pathname new-pathname)
  (with-i/o-condition-adornment
    (lambda ()
      (ik:make-symbolic-link (->fn old-pathname) (->fn new-pathname)))))

(define (file-regular? pathname)
  (ik:file-regular? (->fn pathname)))

(define (file-symbolic-link? pathname)
  (ik:file-symbolic-link? (->fn pathname)))

(define (file-directory? pathname)
  (ik:file-directory? (->fn pathname)))

(define (make-file-check pred who)
  (lambda (pathname)
    (with-i/o-condition-adornment
      (lambda () (pred (->fn pathname))))))

      
;; This re-raising kludge is there to produce an exception of a more
;; specific type; see <https://bugs.launchpad.net/ikarus/+bug/405944>.
(define (with-i/o-condition-adornment thunk)
  (guard (c ((and (i/o-filename-error? c)                  
                  (message-condition? c)
                  (not (or (i/o-file-already-exists-error? c)
                           (i/o-file-does-not-exist-error? c)
                           (i/o-file-is-read-only-error? c)
                           (i/o-file-protection-error? c)))
                  (condition-message->i/o-error-constructor
                   (condition-message c)))
             => (lambda (constructor)
                  (raise (apply condition
                                (map (lambda (c)
                                       (if (i/o-filename-error? c)
                                           (constructor (i/o-error-filename c))
                                           c))
                                     (simple-conditions c)))))))
    (thunk)))

(define (condition-message->i/o-error-constructor msg)
  (cond ((assp (lambda (key)
                  (string-prefix? (string-append key ": ") msg))
                `(("ENOENT" . ,make-i/o-file-does-not-exist-error)
                  ("EEXIST" . ,make-i/o-file-already-exists-error)
                  ("EACCES" . ,make-i/o-file-protection-error)))
         => cdr)
        (else
         #f)))

(define-syntax define-file-check
  (syntax-rules ()
    ((_ id pred)
     (define id (make-file-check pred 'id)))))

(define-file-check file-readable? ik:file-readable?)
(define-file-check file-writable? ik:file-writable?)
(define-file-check file-executable? ik:file-executable?)

(define (file-modification-time pathname)
  (let ((nsecs (ik:file-mtime (->fn pathname))))
    (posix-timestamp->time-utc (div nsecs #e1e9) (mod nsecs #e1e9))))

(define (file-size-in-bytes pathname)
  (ik:file-size (->fn pathname)))

(define directory-stream? ik:directory-stream?)

(define (open-directory-stream pathname)
  (ik:open-directory-stream (->fn pathname)))

(define close-directory-stream ik:close-directory-stream)

(define (read-directory-stream directory-stream)
  (let ((filename (ik:read-directory-stream directory-stream)))
    (if (and filename (or (string=? "." filename)
                          (string=? ".." filename)))
        (read-directory-stream directory-stream)
        filename)))

(define (working-directory)
  (pathname-as-directory (ik:current-directory)))

(define (with-working-directory dir thunk)
  (let ((wd (ik:current-directory)))
    (dynamic-wind
      (lambda () (ik:current-directory
                  (->fn (pathname-as-directory (->pathname dir)))))
      thunk
      (lambda () (ik:current-directory wd)))))

(define library-search-paths ik:library-path)

)
