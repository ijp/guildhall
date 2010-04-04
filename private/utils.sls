;;; utils.sls --- Utilities for dorodango

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (dorodango private utils)
  (export in-hashtable
          symbol<?
          
          wt-tree/update
          
          xvector-remove-first!
          xvector-insert!
          xvector-remove
          in-xvector
          fmt-join/xvector

          apush
          sort-tagged-tree
          
          warn
          fatal
          fatal-error?
          define-guarantor

          opt-ref/list
          dsp-pathname
          pathname-has-file-type?
          pathname-add-type
          pathname->location
          location->pathname
          create-lock-file
          directory->tree
          uri-with-directory-path
          home-pathname
          rm-rf
          logger:dorodango
          make-fmt-log
          fmt-indented)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) drop-right last)
          (srfi :8 receive)
          (srfi :98 os-environment-variables)
          (spells foof-loop)
          (only (spells misc) and=>)
          (spells alist)
          (spells xvector)
          (spells fmt)
          (spells pathname)
          (spells filesys)
          (spells logging)
          (ocelotl wt-tree)
          (ocelotl net uri))

(define-syntax define-guarantor
  (syntax-rules ()
    ((define-guarantor guarantor predicate type-name)
     (define (guarantor obj who)
       (if (predicate obj)
           obj
           (assertion-violation who
                                (string-append "invalid argument type (expected "
                                               type-name ")")
                                obj))))))

(define (warn who message . irritants)
  (raise-continuable
    (condition (make-warning)
               (make-who-condition who)
               (make-message-condition message)
               (make-irritants-condition irritants))))

(define-condition-type &fatal-error &error
  make-fatal-error fatal-error?)

(define (fatal formatter)
  (raise (condition
          (make-fatal-error)
          (make-message-condition (fmt #f formatter)))))

(define (apush k v vals)
  (cond ((assq k vals)
         => (lambda (entry)
              (acons k (cons v (cdr entry)) (remq entry vals))))
        (else
         (acons k (list v) vals))))

(define (opt-ref/list vals key)
  (reverse (or (assq-ref vals key) '())))

(define (sort-tagged-tree tree <?)
  (if (pair? tree)
      (cons (car tree)
            (list-sort
             (lambda (x y)
               (cond ((and (pair? x) (pair? y)) (<? (car x) (car y)))
                     ((pair? x)                 (<? (car x) y))
                     ((pair? y)                 (<? x (car y)))
                     (else                      (<? x y))))
             (map (lambda (element)
                    (sort-tagged-tree element <?))
                  (cdr tree))))
      tree))

(define make-fmt-log
  (case-lambda
    ((logger)
     (let ((log (make-log logger)))
       (lambda (level . formats)
         (log level (lambda (port)
                      (apply fmt port formats))))))
    ((logger level)
     (let ((log (make-log logger level)))
       (lambda formats
         (log (lambda (port) (apply fmt port formats))))))))

(define (fmt-indented indent . formatters)
  (fmt-columns (list (lambda (line) (cat indent line))
                     (apply-cat formatters))))

(define (dsp-pathname pathname)
  (lambda (st)
    ((dsp (->namestring pathname)) st)))

(define (pathname-has-file-type? pathname type)
  (let ((file (pathname-file pathname)))
    (and=> (and file (file-type file))
           (lambda (file-type) (string=? type file-type)))))

(define (pathname-add-type pathname type)
  (let ((file (pathname-file pathname)))
    (pathname-with-file pathname
                        (make-file (file-name file)
                                   (append (file-types file) (list type))))))

(define (pathname->location pathname)
  (append (pathname-directory pathname) (list (file-namestring pathname))))

(define (location->pathname location)
  (make-pathname #f (drop-right location 1) (last location)))

(define (create-lock-file pathname)
  (receive (tmp-file tmp-port)
           (create-temp-file (pathname-with-file pathname ".lk"))
    (call-with-port tmp-port
      (lambda (port) (put-string port "locked")))
    (guard (c ((i/o-file-already-exists-error? c)
               #f))
      (create-hard-link tmp-file pathname)
      (delete-file tmp-file)
      #t)))

(define (maybe-car thing)
  (if (pair? thing) (car thing) thing))

(define (directory->tree pathname)
  (let ((directory (pathname-as-directory pathname)))
    (loop ((for filename (in-directory directory))
           (let pathname (pathname-with-file directory filename))
           (for result
                (listing-reverse
                 (if (file-directory? pathname)
                     (cons filename (directory->tree pathname))
                     filename))))
      => (list-sort (lambda (x y)
                      (string<? (maybe-car x) (maybe-car y)))
                    result))))

;;++ Also in irclogs; need to consolidate
(define (uri-with-directory-path uri)
  (let ((path (uri-path uri)))
    (make-uri (uri-scheme uri)
              (uri-authority uri)
              (if (null? path)
                  '("")
                  (let ((last-elt (last path)))
                    (if (string=? last-elt "")
                        path
                        (append path (list "")))))
              (uri-query uri)
              (uri-fragment uri))))

(define-syntax in-hashtable
  (syntax-rules ()
    ((_ (key-var datum-var) (hashtable-expr) cont . env)
     (cont
      (((keys datums size)                       ;Outer bindings
        (let ((hashtable hashtable-expr))
          (receive (keys datums)
                   (hashtable-entries hashtable)
            (values keys datums (vector-length keys))))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((key-var datum-var)
        (values (vector-ref keys index)
                (vector-ref datums index))))     ;Body bindings
      ()                                         ;Final bindings
      . env))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (home-pathname pathname)
  (pathname-join (pathname-as-directory
                  (get-environment-variable "HOME"))
                 pathname))


;;; xvector utilities

(define (xvector-insert! vec index value)
  (loop continue ((for i (up-from (+ index 1) (to (xvector-length vec))))
                  (with tmp (xvector-ref vec index)))
    => (begin
         (xvector-set! vec index value)
         (xvector-push! vec tmp))
    (let ((new-tmp (xvector-ref vec i)))
      (xvector-set! vec i tmp)
      (continue (=> tmp new-tmp)))))

;; Removes (at most) a single item from xvector; does not keep
;; relative order
(define (xvector-remove-first! vec value =?)
  (loop continue ((for i (up-from 0 (to (xvector-length vec)))))
    (cond ((=? value (xvector-ref vec i))
           (let ((last-element (xvector-pop! vec)))
             (unless (= i (xvector-length vec)) ;shall we remove the last?
               (xvector-set! vec i last-element))))
          (else
           (continue)))))

;; Removes all matching values, and returns a new xvector. Keeps
;; relative order.
(define (xvector-remove vec value =?)
  (let ((result (make-xvector)))
    (loop ((for i (up-from 0 (to (xvector-length vec)))))
      => result
      (let ((elt (xvector-ref vec i)))
        (unless (=? elt value)
          (xvector-push! result elt))))))

(define-syntax in-xvector
  (syntax-rules ()
    ((_ (element-var) (xvector-expr) cont . env)
     (cont
      (((xvector size)                       ;Outer bindings
        (let ((xvector xvector-expr))
          (values xvector (xvector-length xvector)))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((element-var)                            ;Body bindings
        (xvector-ref xvector index)))
      ()                                         ;Final bindings
      . env))))

(define (fmt-join/xvector formatter vec sep)
  (lambda (st)
    (let ((len (xvector-length vec)))
      (loop ((for i (up-from 0 (to len)))
             (with st st
                   ((cat (formatter (xvector-ref vec i))
                         (if (< i (- len 1)) sep fmt-null))
                    st)))
        => st))))

(define (rm-rf pathname)
  (when (and (file-directory? pathname)
             (not (file-symbolic-link? pathname)))
    (let ((directory (pathname-as-directory pathname)))
      (loop ((for filename (in-directory directory)))
        (rm-rf (pathname-with-file directory filename)))))
  (delete-file pathname))


;; This doesn't really belong here
(define logger:dorodango (make-logger root-logger 'dorodango))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
