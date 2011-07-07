;;; gzip.sls --- Bindings for ZLib

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells gzip)
  (export zlib-error?
          compress-bytes
          uncompress-bytes

          open-gz-file-input-port)
  (import (rnrs)
          (only (srfi :43) vector-binary-search)
          (only (spells misc) and=>)
          (spells tracing) (rnrs io simple)
          (spells pathname)
          (spells finite-types)
          (spells foreign))

(define-finite-type zlib-status <zlib-status>
  (code)
  zlib-status?
  zlib-status-vector
  zlib-status/name
  zlib-status/index

  (code zlib-status/code)

  ( ; must be kept in ascending order
   (version-error -6)
   (buf-error -5)
   (mem-error -4)
   (data-error -3)
   (stream-error -2)
   (errno -1)
   (ok 0)
   (stream-end 1)
   (need-dict 2)))

(define (code->zlib-status code)
  (and=>
   (vector-binary-search zlib-status-vector
                         code
                         (lambda (error code)
                           (- (zlib-status/code error)
                              code)))
   (lambda (i)
     (vector-ref zlib-status-vector i))))

(define-condition-type &zlib-error &error
  make-zlib-error zlib-error?
  (z zlib-error/z))

(define (checked-dlopen name)
  (or (dlopen name)
      (error "(spells gzip)" "unable to open shared library" name (dlerror))))

(define libz (checked-dlopen "libz.so.1"))

(define-c-callouts libz
  (%compress 'int "compress" '(pointer pointer pointer long))
  (%uncompress 'int "uncompress" '(pointer pointer pointer long))
  (%gzopen 'pointer "gzopen" '(pointer pointer))
  (%gzread 'int "gzread" '(pointer pointer uint))
  (%gzclose 'int "gzclose" '(pointer)))

(define c-ulong-size (c-type-sizeof 'long))

(define (make-bytes-transformer proc)
  (lambda (dest dest-len source source-len)
    (let*-pointers ((dlen-ptr free (malloc c-ulong-size)))
      (pointer-ulong-set! dlen-ptr 0 dest-len)
      (let ((status (code->zlib-status (proc dest dlen-ptr source source-len))))
        (cond ((eq? status (zlib-status ok))
               (pointer-ulong-ref dlen-ptr 0))
              (else
               status))))))

(define zlib-compress
  (make-bytes-transformer %compress))

(define zlib-uncompress
  (make-bytes-transformer %uncompress))

(define (compress-bytes uncompressed-bytes)
  (let* ((in-len (bytevector-length uncompressed-bytes))
         (out-len (exact (ceiling (+ in-len (* 0.1 in-len) 12)))))
    (let*-pointers ((in-ptr <= uncompressed-bytes)
                    (out-ptr free (malloc out-len)))
      (let ((len/status (zlib-compress out-ptr out-len in-ptr in-len)))
        (if (integer? len/status)
            (memcpy (make-bytevector len/status) 0 out-ptr 0 len/status)
            len/status)))))

(define (uncompress-bytes compressed-bytes uncompressed-size)
  (let ((uncompressed-bytes (make-bytevector uncompressed-size)))
    (let*-pointers ((in-ptr <= compressed-bytes)
                    (out-ptr => uncompressed-bytes))
      (let ((len/status
             (zlib-uncompress out-ptr uncompressed-size
                              in-ptr (bytevector-length compressed-bytes))))
        (if (integer? len/status)
            uncompressed-bytes
            len/status)))))

(define (raise-zlib-error who status message . extra-conditions)
  (raise (apply condition
                (make-who-condition who)
                (make-zlib-error status)
                (make-message-condition message)
                extra-conditions)))


;;; gzip'd ports

(define (zlib-gzopen filename mode)
  (let*-pointers ((fname-ptr free (string->utf8z-ptr filename))
                  (mode-ptr free (string->utf8z-ptr mode)))
    (%gzopen fname-ptr mode-ptr)))

(define (gzfile-read! gzfile lose)
  (lambda (bv start count)
    (let*-pointers ((buf-ptr => bv start (+ start count)))
      (let ((result (%gzread gzfile buf-ptr count)))
        (if (< result 0)
            (lose (code->zlib-status result) "error reading file")
            result)))))

(define (gzfile-close gzfile lose)
  (lambda ()
    (let ((result (code->zlib-status (%gzclose gzfile))))
      (unless (eq? result (zlib-status ok))
        (lose result "error closing file")))))

(define (open-gz-file-input-port pathname)
  (define (lose status message)
    (raise-zlib-error 'open-gz-file-input-port
                      status
                      message
                      (make-i/o-filename-error (->namestring pathname))))
  (let ((gzfile (zlib-gzopen (->namestring pathname) "rb")))
    (when (null-pointer? gzfile)
      (if (= (errno) 0)
          (lose (zlib-status mem-error) "out of memory")
          (lose #f "cannot open input file")))
    (make-custom-binary-input-port "gzip"
                                   (gzfile-read! gzfile lose)
                                   #f ; get-position
                                   #f ; set-position!
                                   (gzfile-close gzfile lose))))

)

;; Local Variables:
;; scheme-indent-styles: ((let*-pointers 1))
;; End:
