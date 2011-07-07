;;; compat.mzscheme.sls --- filesys compat library for MzScheme.

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#lang scheme/base

(provide (rename-out (spells:file-exists? file-exists?)
                     (spells:delete-file delete-file))
         create-directory
         create-symbolic-link
         create-hard-link
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

(require srfi/8
         (lib "spells/pathname.sls")
         (lib "spells/time-lib.sls")
         scheme/foreign
         (only-in scheme/mpair list->mlist)
         (only-in rnrs/io/ports-6
                  make-i/o-filename-error
                  make-i/o-file-already-exists-error
                  make-i/o-file-does-not-exist-error
                  make-i/o-file-protection-error)
         (only-in rnrs/base-6
                  assertion-violation)
         rnrs/records/syntactic-6
         rnrs/conditions-6)

(unsafe!)

(define ->fn ->namestring)

(define (spells:file-exists? pathname)
  (let ((f (->fn pathname)))
    (or (file-exists? f) (directory-exists? f)
        (link-exists? f))))

(define (create-directory pathname)
  (make-directory (->fn pathname)))

(define (make-link-wrapper who c-name)
  (define (lose error-condition)
    (raise (condition
            (make-who-condition who)
            (make-message-condition "OS failure")
            error-condition)))
  (lambda (old-pathname new-pathname)
    (let ((link (get-ffi-obj
                 c-name
                 (ffi-lib #f)
                 (_fun #:save-errno 'posix _string/utf-8 _string/utf-8 -> _int)))
          (old-filename (->fn old-pathname))
          (new-filename  (->fn new-pathname)))
      (let ((rv (link old-filename new-filename)))
        (unless (= 0 rv)
          (case (errno->symbol (saved-errno))
            ((EACCES)
             (lose (make-i/o-file-protection-error new-filename)))
            ((EEXIST)
             (lose (make-i/o-file-already-exists-error new-filename)))
            ((ENOENT)
             (lose (make-i/o-file-does-not-exist-error old-filename)))
            (else
             (lose (make-i/o-filename-error new-filename)))))))))

(define errno->symbol
  (case (system-type 'os)
    ((unix)
     ;; This is actually for Linux
     (lambda (errno)
       (case errno
         ((13) 'EACCES)
         ((17) 'EEXIST)
         ((2)  'ENOENT)
         (else #f))))
    (else
     (assertion-violation 'errno->symbol "unsupported operating system"))))

(define create-symbolic-link
  (make-link-wrapper 'create-symbolic-link "symlink"))

(define create-hard-link
  (make-link-wrapper 'create-hard-link "link"))

(define (spells:delete-file pathname)
  (let ((filename (->fn pathname)))
    (with-exn-converter 'delete-file make-i/o-filename-error filename
      (lambda ()
        (cond ((link-exists? filename)
               (delete-file filename))
              ((directory-exists? filename)
               (delete-directory filename))
              ((file-exists? filename)
               (delete-file filename)))))))

(define (rename-file source-pathname target-pathname)
  (rename-file-or-directory (->fn source-pathname) (->fn target-pathname) #t))

(define (file-regular? pathname)
  (file-exists? (->fn pathname)))

(define (file-symbolic-link? pathname)
  (link-exists? (->fn pathname)))

(define (file-directory? pathname)
  (directory-exists? (->fn pathname)))

(define (make-file-check permission who)
  (lambda (pathname)
    (let ((fname (->fn pathname)))
      (with-exn-converter who make-i/o-file-does-not-exist-error fname
        (lambda ()
          (and (memq permission (file-or-directory-permissions fname))
               #t))))))

(define (with-exn-converter who error-constructor fname thunk)
  (with-handlers
    ((exn:fail:filesystem?
      (lambda (e)
        (raise (condition
                (make-error)
                (make-who-condition who)
                (error-constructor fname)
                (make-message-condition (exn-message e)))))))
    (thunk)))

(define-syntax define-file-check
  (syntax-rules ()
    ((_ id pred)
     (define id (make-file-check pred 'id)))))

(define-file-check file-readable? 'read)
(define-file-check file-writable? 'write)
(define-file-check file-executable? 'execute)

(define (file-modification-time pathname)
  (let ((fname (->fn pathname)))
    (with-exn-converter 'file-modification-time make-i/o-filename-error fname
      (lambda ()
        (posix-timestamp->time-utc
         (file-or-directory-modify-seconds (->fn pathname)))))))

(define (file-size-in-bytes pathname)
  (let ((fname (->fn pathname)))
    (with-exn-converter 'file-size-in-bytes make-i/o-filename-error fname
      (lambda ()
        (file-size fname)))))

;; Emulate the stream with a list. TODO: file a wishlist bug on PLT to
;; support a stream API.
(define-record-type directory-stream
  (fields (mutable entries)))

(define (open-directory-stream pathname)
  (make-directory-stream
   (map path->string (directory-list (->fn pathname)))))

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
  (->pathname (current-directory)))

(define (with-working-directory dir thunk)
  (let ((wd (current-directory)))
    (dynamic-wind
      (lambda () (current-directory
                  (->fn (pathname-as-directory (->pathname dir)))))
      thunk
      (lambda () (current-directory wd)))))

(define (library-search-paths)
  (list->mlist (current-library-collection-paths)))
