;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009 Göran Weinholt <goran@weinholt.se>
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

;; Syntax for defining procedures that calculate Cyclic Redundancy Codes.

;; Ross N. Williams, "A painless guide to CRC error detection
;; algorithms". http://www.ross.net/crc/crcpaper.html

;;; Simple usage with pre-defined CRCs

;; If you want to use one of the pre-defined CRCs

;; (define-crc crc-32)
;;     calculates the CRC table at expand-time and defines the
;;     procedures below

;; (crc-32 bytevector)
;;     returns the final CRC of the entire bytevector
;; (crc-32-init)
;;     returns an initial CRC state
;; (crc-32-update state bv)
;; (crc-32-update state bv start)
;; (crc-32-update state bv start end)
;;     returns a new state which includes the CRC on the given bytes
;; (crc-32-finish state)
;;     returns the final CRC
;; (crc-32-width)
;;     returns the bit-width of the CRC, e.g. 32 for CRC-32
;; (crc-32-self-test)
;;     returns 'sucess, 'failure, or 'no-self-test

;;; Advanced usage

;; Quick and possibly confusing guide to using define-crc with new
;; CRCs, for those who are too busy to read the above paper:

;; Syntax: (define-src name width polynomial init ref-in ref-out
;;                     xor-out check)

;; The width is the bitwise length of the polynomial. You might be
;; lead to believe that it should sometimes be 33, but if so you've
;; been counting the highest bit, which doesn't count.

;; The polynomial for CRC-16 is given sometimes given as x^16 + x^15 +
;; x^2 + 1. This translates to #b1000000000000101 (#x8005). Notice
;; that x^16 is absent. CRCs use polynomial division with modulo two
;; arithmetic (better known as XOR). Don't use the reversed polynomial
;; if you have one of those, instead set ref-in and ref-out properly.

;; After a CRC has been calculated it is sometimes XOR'd with a final
;; value, this is xor-out.

;; check is either the CRC of the ASCII string "123456789", or #f.

;; Syntax: (define-crc name (coefficients ...) init ref-in ref-out
;;                     xor-out check)

;; This is the slightly easier interface where you can simply specify
;; the powers of the coefficients. CRC-16 in this syntax becomes:

;; (define-crc crc-16 (16 15 2 0) #x0000 #t #t #x0000 #xBB3D)

;; Another example: the polynomial x^8 + x^2 + x + 1 in this syntax
;; becomes: (8 2 1 0)

;;; Version history

;; (1 0 20090816) - Initial version. Includes crc-32, crc-16,
;; crc-16/ccitt, crc-32c, and crc-24.

;; (1 0 20090906) - Added crc-64 and the -width procedure. The -update
;; procedure uses fixnums if (> (fixnum-width) (crc-width)).

(library (guild weinholt crypto crc (1 1 20101117))
  (export define-crc)
  (import (rnrs)
          (for (only (srfi :1 lists) iota) expand))

  (define-syntax define-crc
    (lambda (x)
      (define (decode-coefficients coeffs)
        (do ((i coeffs (cdr i))
             (p 0 (bitwise-ior p (bitwise-arithmetic-shift-left 1 (car i)))))
            ((null? i) p)))
      (define (bitwise-reverse-bit-field v start end)
        ;; This is only for the benefit of Ikarus, which does not
        ;; implement this procedure as of 2009-08-16.
        (do ((i start (+ i 1))
             (ret 0 (if (bitwise-bit-set? v i)
                        (bitwise-ior ret (bitwise-arithmetic-shift-left 1 (- end i 1)))
                        ret)))
            ((= i end)
             (bitwise-ior (bitwise-arithmetic-shift-left ret start)
                          (bitwise-copy-bit-field v start end 0)))))
      (define (calc-table index width ref-in poly)
        (if ref-in
            (bitwise-reverse-bit-field (calc-table (bitwise-reverse-bit-field index 0 8)
                                                   width #f poly)
                                       0 width)
            (do ((bit 0 (+ bit 1))
                 (r (bitwise-arithmetic-shift-left index (- width 8))
                    (if (bitwise-bit-set? r (- width 1))
                        (bitwise-xor (bitwise-arithmetic-shift-left r 1) poly)
                        (bitwise-arithmetic-shift-left r 1))))
                ((= bit 8)
                 (bitwise-bit-field r 0 width)))))
      (define (symcat name suffix)
        (datum->syntax name (string->symbol (string-append
                                             (symbol->string (syntax->datum name))
                                             suffix))))
      (syntax-case x ()
        ((_ name)
         ;; Contributions are welcome. There should also be more
         ;; references here. A lot of work went into finding these
         ;; polynomials, and they are reduced to one-liners.
         (case (syntax->datum #'name)
           ;; Used for .ZIP, AUTODIN II, Ethernet, FDDI, PNG, MPEG-2
           ;; and various other things.
           ((crc-32)
            #'(define-crc name 32 #x04C11DB7 #xFFFFFFFF #t #t #xFFFFFFFF #xCBF43926))
           ((crc-16)
            #'(define-crc name 16 #x8005 #x0000 #t #t #x0000 #xBB3D))
           ((crc-16/ccitt)
            ;; Used by XMODEM, PPP and much more
            #'(define-crc name 16 #x1021 #xffff #f #f 0 #x29B1))
           ((crc-32c)
            ;; CRC-32C specified in e.g. RFC4960 or RFC3385. Used by SCTP
            ;; and iSCSI. Finds more errors than CRC-32.
            #'(define-crc name 32 #x1EDC6F41 #xFFFFFFFF #t #t #xFFFFFFFF #xE3069283))
           ;; OpenPGP, see RFC2440.
           ((crc-24)
            #'(define-crc name (24 23 18 17 14 11 10 7 6 5 4 3 1 0)
                          #xB704CE #f #f 0 #x21CF02))
           ((crc-64)
            #'(define-crc name (64 4 3 1 0) 0 #t #t 0 #x46A5A9388A5BEFFE))
           (else
            (syntax-violation #f "this CRC is not pre-defined" #'name))))

        ((_ name (width coeffs ...) . rest)
         (with-syntax ((polynomial (decode-coefficients (syntax->datum #'(coeffs ...)))))
           #'(define-crc name width polynomial . rest)))
        ((_ name width polynomial init ref-in ref-out xor-out check)
         (and (identifier? #'name) (>= (syntax->datum #'width) 8)
              (zero? (mod (syntax->datum #'width) 8)))
         ;; TODO: test different widths. Sub-byte widths need a
         ;; different API.
         (let* ((width* (syntax->datum #'width))
                (polynomial* (syntax->datum #'polynomial))
                (init* (syntax->datum #'init))
                (ref-in* (syntax->datum #'ref-in))
                (ref-out* (syntax->datum #'ref-out)))
           (with-syntax ((mask (- (bitwise-arithmetic-shift-left 1 (- width* 8)) 1))
                         (init (if ref-in*
                                   (bitwise-reverse-bit-field init* 0 width*)
                                   init*))
                         (table (list->vector
                                 (map (lambda (i)
                                        (calc-table i width* ref-in* polynomial*))
                                      (iota 256))))
                         (crc-init (symcat #'name "-init"))
                         (crc-finish (symcat #'name "-finish"))
                         (crc-update (symcat #'name "-update"))
                         (crc-self-test (symcat #'name "-self-test"))
                         (crc-width (symcat #'name "-width")))
             #`(begin
                 (define (name bv)
                   (crc-finish (crc-update (crc-init) bv)))
                 (define (crc-init) init)
                 (define (crc-finish r) (bitwise-xor r xor-out))
                 (define (crc-self-test)
                   (if check
                       (if (= (name (string->utf8 "123456789")) check)
                           'success 'failure)
                       'no-self-test))
                 (define (crc-width) width)
                 (define crc-update
                   (case-lambda
                     ((r* bv)
                      (crc-update r* bv 0 (bytevector-length bv)))
                     ((r* bv start)
                      (crc-update r* bv start (bytevector-length bv)))
                     ((r* bv start end)
                      (define t 'table)
                      (if (> (fixnum-width) width)
                          (do ((i start (+ i 1))
                               (r r*
                                  ;; TODO: implement the other ref-in ref-out combinations?
                                  #,(cond ((and ref-in* ref-out*)
                                           #'(fxxor (fxarithmetic-shift-right r 8)
                                                    (vector-ref
                                                     t (fxxor (fxand #xff r)
                                                              (bytevector-u8-ref bv i)))))
                                          ((and (not ref-in*) (not ref-out*))
                                           #'(fxxor (fxarithmetic-shift-left (fxand mask r) 8)
                                                    (vector-ref
                                                     t (fxxor
                                                        (bytevector-u8-ref bv i)
                                                        (fxand
                                                         (fxarithmetic-shift-right r (- width 8))
                                                         #xff)))))
                                          (else (syntax-violation #f "unimplemented reflection" x)))))
                              ((= i end) r))
                          (do ((i start (+ i 1))
                               (r r*
                                  ;; TODO: implement the other ref-in ref-out combinations?
                                  #,(cond ((and ref-in* ref-out*)
                                           #'(bitwise-xor (bitwise-arithmetic-shift-right r 8)
                                                          (vector-ref
                                                           t (bitwise-xor (bitwise-and r #xff)
                                                                          (bytevector-u8-ref bv i)))))
                                          ((and (not ref-in*) (not ref-out*))
                                           #'(bitwise-xor (bitwise-arithmetic-shift-left (bitwise-and mask r) 8)
                                                          (vector-ref
                                                           t (bitwise-xor
                                                              (bytevector-u8-ref bv i)
                                                              (bitwise-and
                                                               (bitwise-arithmetic-shift-right r (- width 8))
                                                               #xff)))))
                                          (else (syntax-violation #f "unimplemented reflection" x)))))
                              ((= i end) r))))))))))))))
