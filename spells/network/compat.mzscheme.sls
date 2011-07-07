;;; compat.mzscheme.sls --- Network interface, PLT compatibility

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
          (prefix (scheme tcp) mz:))

(define-record-type connection
  (fields input-port output-port))

(define (close-connection conn)
  (close-port (connection-input-port conn))
  (close-port (connection-output-port conn)))

(define listener? mz:tcp-listener?)

(define (listener-accept listener)
  (receive (iport oport) (mz:tcp-accept listener)
    (make-connection iport oport)))

(define (close-listener listener)
  (mz:tcp-close listener))

(define (listener-address listener)
  ;; PLT doesn't support this
  #f)

(define (open-tcp-listener . maybe-options)
  (let-options* (if (null? maybe-options) '() (car maybe-options))
      ((service #f))
    (mz:tcp-listen service)))

(define (open-tcp-connection address service)
  (receive (in-port out-port)
           (mz:tcp-connect address service)
    (make-connection in-port out-port)))

)

;; Local Variables:
;; scheme-indent-styles: ((let-options* 2))
;; End:
