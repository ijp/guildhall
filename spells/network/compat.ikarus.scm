;;; compat.ikarus.sls --- Network interface, Ikarus compatibility

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
          (prefix (only (ikarus)
                        tcp-server-socket
                        accept-connection
                        close-tcp-server-socket
                        tcp-connect)
                  ik:))

(define-record-type connection
  (fields input-port output-port))

(define (close-connection conn)
  (close-port (connection-input-port conn))
  (close-port (connection-output-port conn)))

(define-record-type listener
  (fields socket))

(define (listener-accept listener)
  (receive (iport oport) (ik:accept-connection (listener-socket listener))
    (make-connection iport oport)))

(define (close-listener listener)
  (ik:close-tcp-server-socket (listener-socket listener)))

(define (listener-address listener)
  ;; Ikarus doesn't support this yet
  #f)

(define (open-tcp-listener . maybe-options)
  (let-options* (if (null? maybe-options) '() (car maybe-options))
      ((service #f))
    (unless service
      (raise-impl-restriction 'open-tcp-listener
                              "ephemeral ports not supported"))
    (make-listener (ik:tcp-server-socket service))))

(define (open-tcp-connection address service)
  (receive (in-port out-port)
           (ik:tcp-connect address (cond ((integer? service)
                                          (number->string service))
                                         ((symbol? service)
                                          (symbol->string service))
                                         (else
                                          service)))
    (make-connection in-port out-port)))

)

;; Local Variables:
;; scheme-indent-styles: ((let-options* 2))
;; End:
