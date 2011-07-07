;;; range-tests.sls --- Range test procedures for the AWK macro

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells awk range-tests)
  (export next-range next-:range
          next-range: next-:range:)
  (import (rnrs base))


;;; These procs are for handling RANGE clauses.

;; First return value tells whether this line is active;
;; next value tells whether region is active after this line.
;;
;; (:range  0 4) = 0 1 2 3	This is the most useful one.
;; (range:  0 4) = 1 2 3 4
;; (range   0 4) = 1 2 3
;; (:range: 0 4) = 0 1 2 3 4

;; If these were inlined and the test thunks substituted, it would
;; be acceptably efficient. But who writes Scheme compilers that good
;; in the 90's?

(define (next-:range start-test stop-test state)
  (let ((new-state (if state
		       (or (not (stop-test)) 		; Stop,
			   (start-test))		;   but restart.

		       (and (start-test)		; Start,
			    (not (stop-test))))))	;   but stop, too.
    (values new-state new-state)))

(define (next-range: start-test stop-test state)
  (values state
	  (if state
	      (or (not (stop-test))		; Stop,
		  (start-test))			;   but restart.
	      (and (start-test)			; Start,
		   (not (stop-test))))))	;   but stop, too.

(define (next-range start-test stop-test state)
  (if state
      (let ((not-stop (not (stop-test))))
	(values not-stop				
		(or not-stop			; Stop,
		    (start-test))))		;   but restart.
      (values #f
	      (and (start-test)			; Start,
		   (not (stop-test)))))) 	;   but stop, too.

(define (next-:range: start-test stop-test state)
  (if state
      (values #t
	      (or (not (stop-test))		; Stop
		  (start-test)))		;   but restart.

      (let ((start? (start-test)))
	(values start?
		(and start?			; Start,
		     (not (stop-test)))))))	;   but stop, too.
)
