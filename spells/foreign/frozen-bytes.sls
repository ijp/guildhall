;;; frozen-bytes.sls --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells foreign frozen-bytes)
  (export freeze-bytes
          unfreeze-bytes
          frozen-bytes-pointer)
  (import (rnrs base)
          (rnrs control)
          (rnrs bytevectors)
          (rnrs records syntactic)
          (spells tracing)
          (spells foreign compat))


(define-record-type frozen-bytes
  (fields ptr bv start len))

(define freeze-bytes
  (case-lambda
    ((direction bv start end)
     (let ((len (- end start)))
       (case direction
         ((in)
          (memcpy (malloc len) 0 bv start len))
         ((out)
          (make-frozen-bytes (malloc len) bv start len))
         ((inout)
          (make-frozen-bytes (memcpy (malloc len) 0 bv start len)
                             bv start len))
         (else
          (error 'freeze-bytes "invalid direction" direction)))))
    ((direction bv start)
     (freeze-bytes direction bv start (bytevector-length bv)))
    ((direction bv)
     (freeze-bytes direction bv 0 (bytevector-length bv)))))

(define (frozen-bytes-pointer x)
  (if (frozen-bytes? x)
      (frozen-bytes-ptr x)
      x))

(define (unfreeze-bytes x)
  (cond ((frozen-bytes? x)
         (memcpy (frozen-bytes-bv x) (frozen-bytes-start x)
                 (frozen-bytes-ptr x) 0 (frozen-bytes-len x))
         (free (frozen-bytes-ptr x)))
        (else
         (free x))))

)
