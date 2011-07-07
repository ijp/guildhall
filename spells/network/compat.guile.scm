;;; network.guile.sls --- Network interface, Guile compatibility

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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
          (prefix (only (guile)
                        PF_INET
                        SOCK_STREAM
                        false-if-exception
                        addrinfo:fam
                        addrinfo:addr
                        getaddrinfo
                        socket
                        connect
                        bind
                        listen
                        accept
                        getsockname)
                  guile:))

(define-record-type connection
  (fields socket))

(define (connection-input-port conn)
  (connection-socket conn))

(define (connection-output-port conn)
  (connection-socket conn))

(define (close-connection conn)
  (close-port (connection-socket conn)))

(define-record-type listener
  (fields socket))

(define (listener-accept listener)
  (let ((c (guile:accept (listener-socket listener))))
    (make-connection (car c))))

(define (close-listener listener)
  (close-port (listener-socket listener)))

(define (listener-address listener)
  (guile:getsockname (listener-socket listener)))

(define (socket+sockaddr address service)
  (cond ((guile:false-if-exception
          (car (guile:getaddrinfo address
                                  (cond ((integer? service)
                                         (number->string service))
                                        ((symbol? service)
                                         (symbol->string service))
                                        (else
                                         service)))))
         => (lambda (ai)
              (values (guile:socket (guile:addrinfo:fam ai)
                                    guile:SOCK_STREAM
                                    0)
                      (guile:addrinfo:addr ai))))
        (else
         (values #f #f))))

(define (open-tcp-connection address service)
  (receive (socket sockaddr) (socket+sockaddr address service)
    (unless sockaddr
      (error 'open-tcp-connection
             "cannot resolve address or service"
             address service))
    (guile:connect socket sockaddr)
    (make-connection socket)))

(define (open-tcp-listener . maybe-options)
  (let-options* (if (null? maybe-options) '() (car maybe-options))
      ((service #f)
       (address #f))
    (receive (socket sockaddr) (socket+sockaddr address service)
      (guile:bind socket sockaddr)
      (guile:listen socket 5)
      (make-listener socket))))

)

;; Local Variables:
;; scheme-indent-styles: ((let-options* 2))
;; End:
