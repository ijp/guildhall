#!r6rs
;;; util.sls --- Foreign function interface internal utilities

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(library (spells foreign util)
  (export sized-integer-type
          other-type-aliases)
  (import (rnrs)
          (spells foreign config))

(define sized-integer-type
  (let ((who 'sized-integer-type))
    (case-lambda
      ((ctype signed?)
       (case (c-type-sizeof ctype)
         ((1)  (if signed? 'int8 'uint8))
         ((2)  (if signed? 'int16 'uint16))
         ((4)  (if signed? 'int32 'uint32))
         ((8)  (if signed? 'int64 'uint64))
         (else
          (assertion-violation who
                               "unexpected return value from c-type-sizeof"
                               ctype))))
      ((ctype)
       (let ((signed? (case ctype
                        ((char short int long llong)     #t)
                        ((uchar ushort uint ulong ullong) #f)
                        (else
                         (assertion-violation who
                                              "argument not an integer type"
                                              ctype)))))
         (sized-integer-type ctype signed?))))))

(define other-type-aliases
  `((fpointer . pointer)
    (size_t . ,(sized-integer-type 'size_t #f))
    (ssize_t . ,(sized-integer-type 'ssize_t #t))
    ;; we assume time_t to be a signed integer type; this true at
    ;; least on glibc systems
    (time_t . ,(sized-integer-type 'time_t #t))))

)
