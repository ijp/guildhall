;;; compat.ypsilon.sls --- FFI compat library for Ypsilon

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells foreign compat)
  (export make-pointer-c-getter make-pointer-c-setter

          pointer? null-pointer null-pointer?
          pointer=?
          pointer+

          make-c-callout
          make-c-callback

          errno

          malloc free memcpy memset
          
          dlopen dlsym dlclose dlerror)
  (import (rnrs)
          (srfi :2 and-let*)
          (spells foreign util)
          (spells alist)
          (spells tracing)
          (core)
          (ypsilon ffi))

  (define (todo-proc who)
    (lambda args
      (error who "please implement me!")))

  (define (make-pointer-c-getter sym)
    (define (bv-ref sym)
      (case sym
        ((char)          bytevector-c-int8-ref)
        ((uchar)         bytevector-c-uint8-ref)
        ((short)         bytevector-c-short-ref)
        ((ushort)        bytevector-c-unsigned-short-ref)
        ((int)           bytevector-c-int-ref)
        ((uint)          bytevector-c-unsigned-int-ref)
        ((long)          bytevector-c-long-ref)
        ((ulong)         bytevector-c-unsigned-long-ref)
        ((llong)         bytevector-c-long-long-ref)
        ((ullong)        bytevector-c-unsigned-long-long-ref)
        ((float)         bytevector-c-float-ref)
        ((double)        bytevector-c-double-ref)
        ((pointer)       bytevector-c-void*-ref)
        ((int8)          bytevector-c-int8-ref)
        ((uint8)         bytevector-c-uint8-ref)
        ((int16)         bytevector-c-int16-ref)
        ((uint16)        bytevector-c-uint16-ref)
        ((int32)         bytevector-c-int32-ref)
        ((uint32)        bytevector-c-uint32-ref)
        ((int64)         bytevector-c-int64-ref)
        ((uint64)        bytevector-c-uint64-ref)
        (else #f)))
    (cond ((or (bv-ref sym)
               (bv-ref (resolve-alias sym)))
           => (lambda (ref)
                (lambda (ptr offset)
                  (let ((mapped-bv (make-bytevector-mapping ptr (+ offset 8))))
                    (ref mapped-bv offset)))))
          (else
           (error 'make-pointer-c-getter "invalid type" sym))))
  
  (define (make-pointer-c-setter sym)
    (define (bv-setter sym)
      (case sym
        ((char uchar)    bytevector-c-int8-set!)
        ((short ushort)  bytevector-c-short-set!)
        ((int uint)      bytevector-c-int-set!)
        ((long ulong)    bytevector-c-long-set!)
        ((llong ullong)  bytevector-c-long-long-set!)
        ((float)         bytevector-c-float-set!)
        ((double)        bytevector-c-double-set!)
        ((pointer)       bytevector-c-void*-set!)
        ((int8 uint8)    bytevector-c-int8-set!)
        ((int16 uint16)  bytevector-c-int16-set!)
        ((int32 uint32)  bytevector-c-int32-set!)
        ((int64 uint64)  bytevector-c-int64-set!)
        (else #f)))
    (cond ((or (bv-setter sym)
               (bv-setter (resolve-alias sym)))
           => (lambda (setter)
                (lambda (ptr offset val)
                  (let ((mapped-bv (make-bytevector-mapping ptr (+ offset 8))))
                    (setter mapped-bv offset val)))))
          (else
           (error 'make-pointer-c-setter "invalid type" sym))))
  
  (define (pointer? thing)
    (and (integer? thing) (exact? thing)))
  
  (define (null-pointer) 0)
  
  (define (null-pointer? thing)
    (and (pointer? thing) (= thing 0)))
  
  (define (pointer=? p1 p2)
    (unless (and (pointer? p1) (pointer? p2))
      (error 'pointer=? "invalid arguments" p1 p2))
    (= p1 p2))
  
  (define (pointer+ p offset)
    (unless (and (pointer? p) (integer? offset) (exact? offset))
      (error 'pointer+ "invalid arguments" p offset))
    (+ p offset))

  (define (resolve-alias ctype)
    (cond ((assq-ref other-type-aliases ctype)
           => (lambda (alias)
                (or (resolve-alias alias)
                    alias)))
          (else #f)))

  (define (type->ypsilon-type type)
    (define (prim->ypsilon-type prim)
      (case prim
        ((char)    'int8_t)
        ((uchar)   'uint8_t)
        ((short)   'short)
        ((ushort)  'unsigned-short)
        ((int)     'int)
        ((uint)    'unsigned-int)
        ((long)    'long)
        ((ulong)   'unsigned-long)
        ((llong)   'long-long)
        ((ullong)  'unsigned-long-long)
        ((float)   'float)
        ((double)  'double)
        ((pointer) 'void*)
        ((int8)    'int8_t)
        ((uint8)   'uint8_t)
        ((int16)   'int16_t)
        ((uint16)  'uint16_t)
        ((int32)   'int32_t)
        ((uint32)  'uint32_t)
        ((int64)   'int64_t)
        ((uint64)  'uint64_t)
        ((void)    'void)
        (else #f)))
    (or (prim->ypsilon-type type)
        (and-let* ((alias (resolve-alias type)))
          (prim->ypsilon-type alias))
        (error 'type->ypsilon-type "invalid type" type)))
  
  (define (make-c-callout ret-type arg-types)
    (let ((yp-rt (type->ypsilon-type ret-type))
          (yp-ats (map type->ypsilon-type arg-types)))
      (lambda (ptr)
        (make-cdecl-callout yp-rt yp-ats ptr))))

  (define (make-c-callback ret-type arg-types)
    (let ((yp-rt (type->ypsilon-type ret-type))
          (yp-ats (map type->ypsilon-type arg-types)))
      (lambda (proc)
        (make-cdecl-callback yp-rt yp-ats proc))))

  (define process-dso (load-shared-object))

  (define malloc (c-function process-dso "libc" void* malloc (size_t)))
  (define free (c-function process-dso "libc" void free (void*)))

  (define memcpy
    (case-lambda
      ((p1 offset1 p2 offset2 count)
       (cond ((and (pointer? p1) (bytevector? p2))
              (let ((p1-bv (make-bytevector-mapping (+ p1 offset1) count)))
                (bytevector-copy! p2 offset2 p1-bv 0 count)))
             ((and (bytevector? p1) (pointer? p2))
              (let ((p2-bv (make-bytevector-mapping (+ p2 offset2) count)))
                (bytevector-copy! p2-bv 0 p1 offset1 count)))
             (else
              (error 'memcpy "need pointer and bytevector" p1 p2)))
       p1)
      ((p1 p2 count)
       (memcpy p1 0 p2 0 count))))
  
  (define (memset p v n)
    (let ((p-bv (make-bytevector-mapping p n)))
      (bytevector-fill! p-bv v))
    p)

  (define *dlerror* #f)
  
  (define dlopen
    (case-lambda
      ((lib-name lazy? global?)
       (guard (c (#t (set! *dlerror* c) #f))
         (let ((result (if lib-name
                           (load-shared-object lib-name)
                           (load-shared-object))))
           (set! *dlerror* #f)
           result)))
      ((lib-name)
       (dlopen lib-name #f #f))
      (()
       (dlopen #f #f #f))))

  (define (dlsym lib str)
    (lookup-shared-object lib str))
  
  (define dlclose (todo-proc 'dlclose))

  (define (errno)
    (shared-object-errno))
  
  (define (dlerror)
    *dlerror*)

  (define int->unsigned-int
    (let ((unsigned-int-mask (- (bitwise-arithmetic-shift 1 (* sizeof:int 8)) 1)))
      (lambda (val) (if (< val 0) (bitwise-and val unsigned-int-mask) val))))
  
  (define intptr->uintptr
    (let ((uintptr-mask (- (bitwise-arithmetic-shift 1 (* sizeof:void* 8)) 1)))
      (lambda (val) (if (< val 0) (bitwise-and val uintptr-mask) val))))

)
