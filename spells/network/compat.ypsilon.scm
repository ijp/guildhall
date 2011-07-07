;;; network.ypsilon.sls --- Network interface, Ypsilon compatibility

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells network compat)
  (export connection?
          connection-input-port
          connection-output-port
          close-connection
          
          listener?
          listener-accept
          listener-address
          close-listener

          open-tcp-connection
          open-tcp-listener)
  (import (rnrs)
          (srfi :8 receive)
          (spells network utils)
          (prefix (ypsilon socket) yp:))

(define-record-type connection
  (protocol (lambda (p)
              (lambda (socket)
                (p socket (yp:socket-port socket) (yp:socket-port socket)))))
  (fields socket input-port output-port))

(define (close-connection conn)
  (yp:socket-shutdown (connection-socket conn) yp:SHUT_RDWR))

(define-record-type listener
  (fields socket))

(define (listener-accept listener)
  (make-connection (yp:socket-accept (listener-socket listener))))

(define (close-listener listener)
  (yp:socket-close (listener-socket listener)))

(define (listener-address listener)
  ;; Ypsilon doesn't support this yet
  #f)

(define (open-tcp-connection address service)
  (make-connection (yp:make-client-socket address
                                          (cond ((integer? service)
                                                 (number->string service))
                                                ((symbol? service)
                                                 (symbol->string service))
                                                (else
                                                 service)))))

(define (open-tcp-listener . maybe-options)
  (let-options* (if (null? maybe-options) '() (car maybe-options))
      ((service #f))
    (unless service
      (raise-impl-restriction 'open-tcp-listener
                              "ephemeral ports not supported"))
    (make-listener
     (yp:make-server-socket (number->string service)))))

)

;; Local Variables:
;; scheme-indent-styles: ((let-options* 2))
;; End:
