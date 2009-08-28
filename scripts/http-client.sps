;;; http-client.sps --- Simple HTTP client demo

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Usage: http-client.sps HTTP-URL
;; 
;; Performs a GET request on HTTP-URL, and copies the response body to
;; the standard output port.

;;; Code:
#!r6rs

(import (rnrs)
        (spells network)
        (spells ports)
        (dorodango net http))

(define (main argv)
  (let* ((response (send-http-request (cadr argv)))
         (conn (http-response-data response)))
    (call-with-port (standard-output-port)
      (lambda (oport)
        (copy-port (connection-input-port conn) oport)))
    (close-connection conn)))

(main (command-line))
