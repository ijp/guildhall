;;; compat.ikarus.sls --- filesys compat library for Mosh.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs records syntactic)
          (spells pathname)
          (spells time-lib)
          (prefix (rnrs files) rnrs:)
          (prefix (only (mosh)
                        current-directory set-current-directory!
                        library-path)
                  mosh:)
          (prefix (mosh file) mosh:))

(define ->fn ->namestring)

(define (file-exists? pathname)
  (rnrs:file-exists? (->fn pathname)))

(define (create-directory pathname)
  (mosh:create-directory (->fn pathname)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (mosh:delete-directory (->fn pathname))
          (rnrs:delete-file (->fn pathname)))))

(define (rename-file source-pathname target-pathname)
  (mosh:rename-file (->fn source-pathname) (->fn target-pathname)))

(define (not-implemented procedure)
  (raise (condition
          (make-implementation-restriction-violation)
          (make-who-condition procedure)
          (make-message-condition "Not yet implemented on Mosh Scheme"))))

(define (create-hard-link old-pathname new-pathname)
  (not-implemented 'create-hard-link))

(define (create-symbolic-link old-pathname new-pathname)
  (mosh:create-symbolic-link (->fn old-pathname) (->fn new-pathname)))

(define (file-regular? pathname)
  (mosh:file-regular? (->fn pathname)))

(define (file-symbolic-link? pathname)
  (mosh:file-symbolic-link? (->fn pathname)))

(define (file-directory? pathname)
  (mosh:file-directory? (->fn pathname)))

(define (file-readable? pathname)
  (mosh:file-readable? (->fn pathname)))
(define (file-writable? pathname)
  (mosh:file-writable? (->fn pathname)))
(define (file-executable? pathname)
  (mosh:file-executable? (->fn pathname)))

(define (file-modification-time pathname)
  (let ((nsecs (mosh:file-stat-mtime (->fn pathname))))
    (posix-timestamp->time-utc (div nsecs #e1e9) (mod nsecs #e1e9))))

(define (file-size-in-bytes pathname)
  (mosh:file-size-in-bytes (->fn pathname)))

;; Emulate the stream with a list. TODO: file a wishlist bug on mosh
;; to support a stream API.
(define-record-type directory-stream
  (fields (mutable entries)))

(define (open-directory-stream pathname)
  (make-directory-stream (mosh:directory-list (->fn pathname))))

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
  (->pathname (mosh:current-directory)))

(define (with-working-directory dir thunk)
  (let ((wd (mosh:current-directory)))
    (dynamic-wind
      (lambda () (mosh:set-current-directory!
                  (->fn (pathname-as-directory (->pathname dir)))))
      thunk
      (lambda () (mosh:set-current-directory! wd)))))

(define library-search-paths mosh:library-path)

)
