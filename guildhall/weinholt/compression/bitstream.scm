;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; Read bits from binary input ports.

(library (guildhall weinholt compression bitstream (1 0 20101007))
  (export make-bit-reader
          get-bits lookahead-bits align-bit-reader
          get-bit-reader-buffer)
  (import (rnrs))

  ;; (define-record-type bit-reader
  ;;   (fields (immutable port)
  ;;           (mutable buf)
  ;;           (mutable buflen))
  ;;   (protocol
  ;;    (lambda (p)
  ;;      (lambda (port)
  ;;        (p port 0 0)))))

  ;; Records are too slow in Ikarus...
  (define (make-bit-reader port) (vector port 0 0))
  (define (bit-reader-port br) (vector-ref br 0))
  (define (bit-reader-buf br) (vector-ref br 1))
  (define (bit-reader-buflen br) (vector-ref br 2))
  (define (bit-reader-buf-set! br v) (vector-set! br 1 v))
  (define (bit-reader-buflen-set! br v) (vector-set! br 2 v))

  (define (fill! br n)
    (let lp ()
      (let ((buflen (bit-reader-buflen br)))
        (when (fx<? buflen n)           ;read more?
          (bit-reader-buf-set! br (fxior (fxarithmetic-shift-left
                                          (get-u8 (bit-reader-port br))
                                          buflen)
                                         (bit-reader-buf br)))
          (bit-reader-buflen-set! br (fx+ buflen 8))
          (lp)))))

  (define (read! br n)
    (let ((buf (bit-reader-buf br))
          (buflen (bit-reader-buflen br)))
      (bit-reader-buf-set! br (fxarithmetic-shift-right buf n))
      (bit-reader-buflen-set! br (fx- buflen n))
      (fxbit-field buf 0 n)))

  ;; Read n bits from the port
  (define (get-bits br n)
    (fill! br n)
    (read! br n))

  ;; Peek at n bits from the port
  (define (lookahead-bits br n)
    (fill! br n)
    (fxbit-field (bit-reader-buf br) 0 n))

  ;; Not called very often. It is used to discard all bits up until
  ;; the next byte boundary.
  (define (align-bit-reader br)
    (let ((buflen (bit-reader-buflen br)))
      (read! br (fx- buflen (fxand buflen -8)))))

  ;; Return the buffer as a bytevector.
  (define (get-bit-reader-buffer br)
    (let ((buf (bit-reader-buf br))
          (buflen (bit-reader-buflen br)))
      (bit-reader-buf-set! br 0)
      (bit-reader-buflen-set! br 0)
      (if (fxzero? buflen)
          #vu8()
          (let ((bv (make-bytevector (fxdiv (fxand -8 (fx+ 7 buflen)) 8))))
            (bytevector-uint-set! bv 0 buf (endianness big)
                                  (bytevector-length bv))
            bv)))))
