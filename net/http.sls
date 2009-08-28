;;; http.sls --- Simple HTTP client library

;; Copyright (C) 2005,2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (dorodango net http)
  (export send-http-request

          make-simple-http-request
          make-http-request

          http-response-code
          http-response-version
          http-response-header
          http-response-data)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :14)
          (only (srfi :13 strings)
                string-concatenate
                string-index
                string-prefix?
                string-skip
                string-trim-both)
          (spells string-utils)
          (spells network))

;;; Utilities

(define (put-utf8-crlf port . args)
  (let loop ((args args))
    (if (null? args)
        (put-bytevector port '#vu8(13 10))
        (let ((obj (car args)))
          (cond ((string? obj)
                 (put-bytevector port (string->utf8 obj)))
                (else
                 (put-bytevector port obj)))
          (loop (cdr args))))))

(define (:optional maybe-arg default)
  (if (pair? maybe-arg)
      (if (null? (cdr maybe-arg))
          (car maybe-arg)
          (assertion-violation ':optional "too many optional arguments"))
      default))

(define (bytevector-slice bv start end)
  (if (and (= start 0)
           (= end (bytevector-length bv)))
      bv
      (let ((result (make-bytevector (- end start))))
        (bytevector-copy! bv start result 0 (- end start))
        result)))

(define (read-crlf-line port)
  (let loop ((buffer (make-bytevector 128))
             (len 0)
             (had-cr? #f))
    (let ((u8 (get-u8 port)))
      (define (store+iterate cr?)
        (let* ((size (bytevector-length buffer))
               (buffer (if (>= len size)
                           (let ((new-buffer (make-bytevector (*  2))))
                             (bytevector-copy! buffer 0 new-buffer 0 len)
                             new-buffer)
                           buffer)))
          (bytevector-u8-set! buffer len u8)
          (loop buffer (+ len 1) cr?)))
      (cond ((or (eof-object? u8)
                 (= u8 ascii:lf)) ;; we also fire on linefeed by itself
             (if (and (eof-object? u8)
                      (= len 0))
                 (eof-object)
                 (utf8->string (bytevector-slice buffer 0 (- len (if had-cr? 1 0))))))
            ((= u8 ascii:cr)
             (store+iterate #t))
            (else
             (store+iterate #f))))))

;;; Simple RFC-822 header parser

;; Code derived from SUnet's rfc822.scm (new-style BSD license):
;; Copyright (c) 1995 by Olin Shivers <shivers@lcs.mit.edu>
;; Copyright (c) 2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>

(define ascii:space 32)
(define ascii:tab 9)
(define ascii:cr 13)
(define ascii:lf 10)

(define (read-rfc822-field port)
  (receive (field body) (read-rfc822-field-with-line-breaks port)
    (values field
            (string-concatenate body))))

(define (read-rfc822-field-with-line-breaks port)
  (define who 'read-rfc822-field-with-line-breaks)
  (let ((line1 (read-crlf-line port)))
    (cond ((or (eof-object? line1)
               (zero? (string-length line1)))
           (values #f #f))
          ((string-index line1 #\:) =>
           (lambda (colon)
             (let ((name (string->symbol (substring line1 0 colon))))
               ;; Read in continuation lines.
               (let loop ((lines (list (substring line1
                                                  (+ colon 1)
                                                  (string-length line1)))))
                 (let ((u8 (lookahead-u8 port)))
                   ;;  RFC822: continuous lines has to start with a space or a htab
                   (if (or (eqv? u8 ascii:space)
                           (eqv? u8 ascii:tab))
                       (loop (cons (read-crlf-line port) lines))
                       (values name (reverse lines))))))))
          (else
           (error who "illegal RFC 822 field syntax" line1)))))

(define (make-read-rfc822-headers read-field)
  (lambda (port)
    (let loop ((alist '()))
      (receive (field val) (read-field port)
        (if field
            (loop (cons (cons field val) alist))
            (reverse alist))))))

(define read-rfc822-headers
  (make-read-rfc822-headers read-rfc822-field))

;;; Request objects

(define-record-type (http-request %make-http-request http-request?)
  (fields method uri version headers data))

(define (make-http-request method uri version headers data)
  (let* ((data (cond ((string? data) (string->utf8 data))
                     ((bytevector? data) data)
                     (else #f)))
         (content-length (and (not (assq 'content-length headers))
                              data
                              (bytevector-length data))))
    (%make-http-request method uri version
                        (if content-length
                            (cons `(content-length . ,content-length)
                                  headers)
                            headers)
                        data)))

(define (make-simple-http-request method uri version headers data)
  (define who 'make-simple-http-request)
  (receive (protocol host port path) (parse-http-uri uri)
      (or host
          (assertion-violation who "no host given in HTTP URI" uri))
      (make-http-request
       method (or path "/") version
       (cons `(host . ,(if port
                           (string-append host ":" (number->string port))
                           host))
             headers)
       data)))

(define (http-request-header request header . maybe-default)
  (cond ((assq header (http-request-headers request))
         => (lambda (entry) (string-trim-both (cdr entry))))
        (else
         (and (not (null? maybe-default)) (car maybe-default)))))

;;; Response objects

(define-record-type http-response
  (fields version code message headers data))

(define (http-response-header response header . maybe-default)
  (cond ((assq header (http-response-headers response))
         => (lambda (entry) (string-trim-both (cdr entry))))
        (else
         (and (not (null? maybe-default)) (car maybe-default)))))

;;; Request sending

(define (read-http-status-line in-port)
  (let ((line (read-crlf-line in-port)))
    (if (eof-object? line)
        ;; TODO: make this a condition
        (error 'read-http-status-line "EOF while reading HTTP status line"))
    (let ((parts (string-split line #\space 3)))
      (apply
       values
       ;; Poor man's regular expression
       (or (and-let* (((>= (length parts) 2))
                      ((= (string-length (car parts)) 8))
                      ((string-prefix? "HTTP/" (car parts)))
                      ((char=? #\. (string-ref (car parts) 6)))
                      (major (string->number (substring (car parts) 5 6)))
                      (minor (string->number (substring (car parts) 7 8)))
                      (status (string->number (cadr parts))))
             (list (cons major minor)
                   status
                   (and (= (length parts) 3) (caddr parts))))
           (list #f #f #f))))))

(define host-charset
  (char-set-union char-set:letter+digit
                  (->char-set "-._")))

(define (parse-http-uri uri)
  (apply
   values
   (or (and-let* ((host-start (if (string-prefix? "http://" uri) 7 0))
                  (host-end (string-skip uri host-charset host-start))
                  (path-start
                   (if (char=? #\: (string-ref uri host-end))
                       (string-skip uri char-set:digit (+ host-end 1))
                       host-end)))
         (list 'http
               (substring uri host-start host-end)
               (and (< host-end path-start)
                    (string->number (substring uri (+ host-end 1) path-start)))
               (substring uri path-start (string-length uri))))
       (list #f #f #f #f))))

(define (write-http-request-data port request)
  (let ((data (http-request-data request)))
    (cond ((string? data)
           (put-bytevector port (string->utf8 data)))
          ((bytevector? data)
           (put-bytevector port data))
          ((procedure? data)
           (data port))
          (else
           (error 'write-http-request-data
                  "don't know how to send HTTP request data" data)))))

(define (field-name->string name)
  (symbol->string name)) ;; Prettify?

(define (field-value->string value)
  (cond ((string? value) value)
        ((number? value) (number->string value))
        (else
         (error 'field-value->string
                "don't know how to stringify HTTP headers field" value))))

(define (version->string v)
  (string-append "HTTP/"
                 (number->string (car v))
                 "."
                 (number->string (cdr v))))


(define (http-request->connection request)
  (define who 'http-request->connection)
  (let*-values
      (((host-header) (http-request-header request 'host))
       ((host port)
        (cond ((string-index host-header #\:)
               => (lambda (pos)
                    (values
                      (substring host-header 0 pos)
                      (string->number
                       (substring host-header (+ pos 1)
                                  (string-length host-header))))))
              (else (values host-header #f)))))
    (if host
        (open-tcp-connection host (or port 80))
        (assertion-violation who
                             "no host specified in request"
                             request))))

(define (send-http-request request . maybe-connection)
  (let ((request (if (string? request)
                     (make-simple-http-request "GET" request '(1 . 0) '() '#vu8())
                     request)))
    (let* ((connection
            (or (:optional maybe-connection #f)
                (http-request->connection request)))
           (in (connection-input-port connection))
           (out (connection-output-port connection)))
      (put-utf8-crlf
       out
       (http-request-method request) " " (http-request-uri request) " "
       (version->string (http-request-version request)))
      (for-each (lambda (pr)
                  (put-utf8-crlf out
                                 (field-name->string (car pr))
                                 ": "
                                 (field-value->string (cdr pr))))
                (http-request-headers request))
      (put-utf8-crlf out)
      (write-http-request-data out request)
      (flush-output-port out)
      (let*-values (((version code message) (read-http-status-line in))
                    ((headers) (read-rfc822-headers in)))
        (make-http-response version code message headers connection)))))

)
