#!r6rs
;;; network.sls --- Network interface

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ TCP networking interface.
(library (spells network)
  (export connection?
          connection-input-port
          connection-output-port
          close-connection
          
          listener?
          listener-address
          listener-accept
          close-listener
          
          listener-accept-loop

          open-tcp-connection
          open-tcp-listener)
  (import (rnrs)
          (srfi :8 receive)
          (spells network compat))

;;;@subsection Connections
;;@anchor{Network connections}

;; A connection object represents one end of a two-way reliable
;; communication channel established between two transport addresses.

;;@defun connection? thing
;;   Disjoint type predicate for connection objects. 
;;@end defun

;;@defun connection-input-port connection
;;@defunx connection-output-port connection

;; These return input & output ports suitable for sending & receiving
;; data to and from the connection represented by @var{connection}.
;; If either port is closed, the respective direction of the
;; connection is shut down.

;;@end defun

;;@defun connection-local-address
;;@defunx connection-remote-address connection

;; These return the transport addresses of the connection. Not all
;; connection types support these operations, in which case @code{#f}
;; is returned.

;;@end defun

;;@defun close-connection connection

;; Closes communication on @var{connection}.  This automatically
;; flushes any buffered output first on @var{connection}'s output port
;; and shuts down either direction of the connection if necessary.
;; Note that closing both the input & output ports may not have the
;; same effect, and connections should be fully closed only with
;; @code{close-connection}; no guarantee is made that this will occur
;; if both of the ports are closed.

;;@end defun

;;;@subsubsection TCP connections

;;@defun open-tcp-connection address service [options]

;; Creates a TCP connection to the TCP socket specified by
;; @var{address} and @var{service}. @var{address} may have a number of
;; different forms.  If it is a four-octet SRFI 74 blob or a
;; dot-delimited string of four decimal octets, it is an IPv4 address.
;; This form is required to be implemented.  If @code{(tcp-ipv6?)}
;; returns true, @var{address} may also be a sixteen-octet SRFI 74
;; blob or a string in the colon-delimited hexadecimal syntax of IPv6,
;; in which case it represents an IPv6 address.  Finally,
;; @var{address} may be a string for a named host, which is resolved
;; by DNS.  Implementations may extend the form of @var{address}.
;; @var{service} may either be an integer in the range [0,65536), a
;; string, or a symbol.  For example,

;; @lisp
;; (open-tcp-connection "www.symbolics.com" 'http)
;; @end lisp

;; would try to open a TCP connection to @samp{www.symbolics.com} on
;; the HTTP service, i.e. service number 80.

;; If the optional argument @var{options} is given, it should be an
;; alist of options for the created connection. The following options
;; are defined:

;; @table @code
;; @item (local-address @var{address-blob})
;; @itemx (local-service @var{service})
;;
;; Specifies the address and port the local end of the connection will be bound to. 

;; @item (address-resolver @var{resolve-proc})
;; 
;; If this option is present, @var{resolve-proc} will be called with
;; the @var{address} passed to @code{open-tcp-client}, if
;; @var{address} not represented as SRFI 74 blobs or in the numerical
;; string representation. @var{resolve-proc} must either signal a
;; condition, or return a single address blob value.
;;
;; @end table
;;@end defun

;;;@subsection Listeners

;;@defun listener? object
;;
;; Return @code{#t} if @var{object} is a listener, and @code{#f}
;; otherwise.
;;
;;@end defun
  
;;@defun listener-address listener
;;
;; Return the transport address of @var{listener}.
;;
;;@end defun

;;@defun listener-accept listener
;;
;; Accept a connection.  This procedure blocks until a connection is
;; made to @var{listener} and returns the corresponding connection
;; object (@pxref{Network Connections,,Connections}).
;;
;;@end defun

;;@defun close-listener listener
;; Close @var{listener}, freeing the associated resources.
;;@end defun

;;@ Create a listener and accept connections in a loop.
(define (listener-accept-loop open initializer receiver . seeds)
  (let ((listener #f))
    (dynamic-wind
      (lambda () (set! listener (open)))
      (lambda ()
        (initializer (listener-address listener))
        (let loop ((seeds seeds))
          (let ((connection (listener-accept listener)))
            (receive (continue? . seeds) (apply receiver connection seeds)
              (close-connection connection)
              (cond (continue?
                     (loop seeds))
                    (else
                     (apply values seeds)))))))
      (lambda () (close-listener listener)))))

;;;@subsubsection TCP listeners

;;@defun open-tcp-listener [options]

;; Opens a TCP listener.  If @var{options} is supplied, it should be
;; an alist of options for the TCP listener.  The following options
;; are defined:

;; @table @code
;; @item (service @var{tcp-service})

;; The listener listens on the given TCP service.  If this is not
;; supplied, an unspecified one is chosen by the operating system;
;; this may be queried with @code{tcp-listener-service}.

;; @item (address @var{ip-address})

;; The listener listens only for connections from the given address,
;; which must be of a form as described in OPEN-TCP-CLIENT.  If this
;; is unsupplied, it defaults to the TCP `any' address, which allows
;; connections from any address.

;; @item (max-waiters @var{n})

;; Specifies that a maximum of n client connections may be waiting to
;; be accepted by the listener before further connection requests are
;; refused.  If @var{n} is @code{#f} or this option is not supplied,
;; the default is an implementation-defined value.
;;
;; @end table
;;@end defun
)
