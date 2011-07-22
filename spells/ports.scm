#!r6rs
(library (spells ports)
  (export copy-port)
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs io ports)
          (only (guile) define*))

  (define* (copy-port in-port out-port #:optional
                      (buffer-size 4000) (thunk (lambda () #f)))
    (cond ((and (binary-port? in-port)
                (binary-port? out-port))
           (let ((buffer (make-bytevector buffer-size 0)))
             (let loop ()
               (let ((n (get-bytevector-n! in-port buffer 0 buffer-size)))
                 (cond ((not (eof-object? n))
                        (put-bytevector out-port buffer 0 n)
                        (thunk)
                        (loop)))))))
          ((and (textual-port? in-port)
                (textual-port? out-port))
           (let ((buffer (make-string buffer-size)))
             (let loop ()
               (let ((n (get-string-n! in-port buffer 0 buffer-size)))
                 (cond ((not (eof-object? n))
                        (put-string out-port buffer 0 n)
                        (thunk)
                        (loop)))))))
          (else
           (error 'copy-port
                  "provided ports must be both binary or both textual"
                  in-port out-port)))))
