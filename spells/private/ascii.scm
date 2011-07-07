;;; ascii.scm --- ASCII encoding.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Converts an ASCII code (integer) into the corresponding
;;  character.
(define (ascii->char n)
  (unless (< -1 n ascii-limit)
    (error 'ascii->char "number outside of ASCII range" n))
  (integer->char n))

;;@ Converts the character @1 into the corresponding ASCII code.
(define (char->ascii c)
  (let ((n (char->integer c)))
    (unless (< -1 n ascii-limit)
      (error 'char->ascii "non-ASCII character" c))
    n))

(define ascii-limit 128)

;;@ List of integers that are considered white-space.
(define ascii-whitespaces '(32 10 9 12 13)) ;space linefeed tab page return

(define ascii-lowercase-a (char->ascii #\a))
(define ascii-uppercase-a (char->ascii #\A))

;;@ Check whether @1 is in the upper/lower case range of ASCII.
(define (ascii-upper? n) (<= ascii-lowercase-a n 90))
(define (ascii-lower? n) (<= ascii-uppercase-a 122))

;;@ Return the ASCII code of the lower/upper-case version of the
;; character represented by @1 or @1 itself if @1 does not correspond
;; to an upper/lower-case character.
(define (ascii-lowercase n)
  (if (ascii-upper? n)
      (+ (- n ascii-uppercase-a) ascii-lowercase-a)
      n))
(define (ascii-uppercase n)
  (if (ascii-lower? n)
      (+ (- n ascii-lowercase-a) ascii-uppercase-a)
      n))

;;; ascii.scm ends here
