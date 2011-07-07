;;; compat.ypsilon.sls --- filesys compat library for Ypsilon.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs


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
  (import (rnrs base)
          (rnrs records syntactic)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs io ports)
          (prefix (rnrs files) rnrs:)
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)
          (prefix (core files) yp:)
          (prefix (core primitives) yp:))

  (define ->fn ->namestring)

  (define (file-exists? pathname)
    (rnrs:file-exists? (->fn pathname)))

  (define (create-directory pathname)
    (yp:create-directory (->fn pathname)))

  (define (create-symbolic-link old-pathname new-pathname)
    (yp:create-symbolic-link (->fn old-pathname) (->fn new-pathname)))

  (define (create-hard-link old-pathname new-pathname)
    (yp:create-hard-link (->fn old-pathname) (->fn new-pathname)))

  (define (delete-file pathname)
    (let ((fname (->fn pathname)))
      (if (rnrs:file-exists? fname)
          (yp:delete-file fname))))

  (define (rename-file source-pathname target-pathname)
    (yp:rename-file (->fn source-pathname) (->fn target-pathname)))

  (define (file-regular? pathname)
    (yp:file-regular? (->fn pathname)))
  (define (file-directory? pathname)
    (yp:file-directory? (->fn pathname)))
  (define (file-symbolic-link? pathname)
    (yp:file-symbolic-link? (->fn pathname)))

  (define (make-file-check pred who)
    (lambda (pathname)
      (let ((fname (->fn pathname)))
        (if (rnrs:file-exists? fname)
            (pred fname)
            (raise (condition
                    (make-error)
                    (make-who-condition who)
                    (make-i/o-file-does-not-exist-error fname)))))))

  (define-syntax define-file-check
    (syntax-rules ()
      ((_ id pred)
       (define id (make-file-check pred 'id)))))
  
  (define-file-check file-readable? yp:file-readable?)
  (define-file-check file-writable? yp:file-writable?)
  (define-file-check file-executable? yp:file-executable?)

  (define (file-modification-time pathname)
    (let ((nsecs (yp:file-stat-mtime (->fn pathname))))
      (posix-timestamp->time-utc (div nsecs #e1e9) (mod nsecs #e1e9))))

  (define (file-size-in-bytes pathname)
    (yp:file-size-in-bytes (->fn pathname)))

  ;; Emulate the stream with a list, until
  ;; <http://code.google.com/p/ypsilon/issues/detail?id=124> is
  ;; resolved.
  (define-record-type directory-stream
    (fields (mutable entries)))

  (define (open-directory-stream pathname)
    (make-directory-stream (yp:directory-list (->fn pathname))))
  
  (define (close-directory-stream stream)
    (directory-stream-entries-set! stream #f))

  (define (read-directory-stream stream)
    (let ((entries (directory-stream-entries stream)))
      (if (null? entries)
          #f
          (let ((filename (car entries)))
            (directory-stream-entries-set! stream (cdr entries))
            (if (or (string=? "." filename)
                    (string=? ".." filename))
                (read-directory-stream stream)
                filename)))))
  
  (define (working-directory)
    (pathname-as-directory (yp:current-directory)))

  (define (with-working-directory dir thunk)
    (let ((wd (yp:current-directory)))
      (dynamic-wind
        (lambda () (yp:current-directory
                    (->fn (pathname-as-directory (->pathname dir)))))
        thunk
        (lambda () (yp:current-directory wd)))))


  (define library-search-paths yp:scheme-library-paths)

  )
