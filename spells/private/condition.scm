;;; condition.scm --- Additional condition types and utilities.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Comentary:

;;
;; These are common condition types which can be shared among
;; libraries (instead of each re-defining them).

;;; Code:


(define-condition-type &parser-error &error
  make-parser-error parser-error?
  (port parser-error-port))

(define-condition-type &stacked &condition
  make-stacked-condition stacked-condition?
  (next next-condition))

(define-syntax formatting
  (syntax-rules ()
    ((_ (state-var) (formatter state-expr) cont . env)
     (cont
      ()                                         ;Outer bindings
      ((state-var state-expr                     ;Loop variables
                  (formatter state-expr)))
      ()                                         ;Entry bindings
      ()                                         ;Termination conditions
      ()                                         ;Body bindings 
      ()                                         ;Final bindings
      . env))))

(define (dsp-simple-condition c)
  (define (dsp-rtd.fields-list rtd.fields-list n-fields)
    (case n-fields
      ((0) nl)
      ((1)
       (cat ": " (wrt/unshared ((record-accessor (caar rtd.fields-list) 0) c)) nl))
      (else
       (cat ":\n"
            (fmt-join
             (lambda (rtd.fields)
               (dsp-fields (car rtd.fields) (cdr rtd.fields)))
             rtd.fields-list
             "\n")))))
  (define (dsp-fields rtd fields)
    (lambda (st)
      (loop ((for i (up-from 0 (to (vector-length fields))))
             (for st (formatting
                      (cat "      "
                           (vector-ref fields i) ": "
                           (wrt/unshared ((record-accessor rtd i) c)) "\n")
                      st)))
        => st)))
  (lambda (st)
    (let ((c-rtd (record-rtd c)))
      (loop ((with rtd c-rtd (record-type-parent rtd))
             (while rtd)
             (for rtd.fields-list
                  (listing (cons rtd (record-type-field-names rtd))))
             (for n-fields (summing (vector-length
                                     (record-type-field-names rtd)))))
        => ((cat (record-type-name c-rtd)
                 (dsp-rtd.fields-list
                  (remp (lambda (rtd.fields)
                          (zero? (vector-length (cdr rtd.fields))))
                        rtd.fields-list)
                  n-fields))
            st)))))

(define (dsp-condition c)
  (define (dsp-components components)
    (lambda (st)
      (loop ((for c (in-list components))
             (for i (up-from 1))
             (for st (formatting
                      (cat "  " i ". " (dsp-simple-condition c))
                      st)))
        => st)))
  (cond
    ((condition? c)
     (let ((components (simple-conditions c)))
       (if (null? components)
           (dsp "Condition object with no further information\n")
           (cat "Condition components:\n"
                (dsp-components components)))))
    (else
     (cat "Non-condition object: " c (wrt/unshared c) "\n"))))

(define display-condition
  (case-lambda
    ((c port)
     (fmt port (fmt-columns (list (lambda (line) (cat " " line))
                                  (dsp-condition c)))))
    ((c)
     (display-condition c (current-output-port)))))


;; Utilities for code below. This whole section should probably be
;; moved to its own library.

(define (write-string s port)
  (put-string port s))

;; Code below taken from scheme48 1.8, Copyright (c) 1993-2008 Richard
;; Kelsey and Jonathan Rees. Licensed under the new-style BSD license.

(define (limited-write obj port max-depth max-length)
  (let recur ((obj obj) (depth 0))
    (if (and (= depth max-depth)
	     (not (or (boolean? obj)
		      (null? obj)
		      (number? obj)
		      (symbol? obj)
		      (char? obj)
		      (string? obj))))
	(display "#" port)
	(call-with-current-continuation
	  (lambda (escape)
	    (recurring-write obj port
	      (let ((count 0))
		(lambda (sub)
		  (if (= count max-length)
		      (begin (display "---" port)
			     (write-char
			      (if (or (pair? obj) (vector? obj))
				  #\)
				  #\})
			      port)
			     (escape #t))
		      (begin (set! count (+ count 1))
			     (recur sub (+ depth 1))))))))))))

(define (recurring-write obj port recur)
  (cond ((pair? obj) (write-list obj port recur))
        ((vector? obj) (write-vector obj port recur))
        (else
         (write obj port))))

(define (write-list obj port recur)
  (cond ((quotation? obj)
         (write-char #\' port)
         (recur (cadr obj)))
        (else
         (write-char #\( port)
         (recur (car obj))
         (let loop ((l (cdr obj))
                    (n 1))
              (cond ((not (pair? l))
                     (cond ((not (null? l))
                            (write-string " . " port)
                            (recur l))))
                    (else
                      (write-char #\space port)
                      (recur (car l))
                      (loop (cdr l) (+ n 1)))))
         (write-char #\) port))))

(define (quotation? obj)
  (and (pair? obj)
       (eq? (car obj) 'quote)
       (pair? (cdr obj))
       (null? (cddr obj))))

(define (write-vector obj port recur)
   (write-string "#(" port)
   (let ((z (vector-length obj)))
     (cond ((> z 0)
            (recur (vector-ref obj 0))
            (let loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space port)
                     (recur (vector-ref obj i))
                     (loop (+ i 1))))))))
   (write-char #\) port))

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:


