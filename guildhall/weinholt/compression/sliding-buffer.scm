;;; sliding-buffer.scm --- A circular buffer attached to a data sink

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2010 Göran Weinholt <goran@weinholt.se>

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

;; Modified on 2010-04-17 by Göran Weinholt <goran@weinholt.se>
;;  Fixed a bug where sliding-buffer-dup! would try to do a
;;  bytevector-copy! beyond the end of sliding-buffer-data. Also
;;  implemented sliding-buffer-init! for use by ZLIB's pre-set
;;  dictionaries.

;;; Code:
#!r6rs

(library (guildhall weinholt compression sliding-buffer)
  (export make-sliding-buffer
          sliding-buffer?
          sliding-buffer-init!
          sliding-buffer-drain!
          sliding-buffer-read!
          sliding-buffer-put-u8!
          sliding-buffer-dup!)
  (import (rnrs))

  (define-record-type sliding-buffer
    (protocol (lambda (p)
                (lambda (sink size)
                  (p sink (make-bytevector size) 0 0))))
    (fields sink
            data
            (mutable fill)
            (mutable pos)))

  (define (sliding-buffer-size buffer)
    (bytevector-length (sliding-buffer-data buffer)))
  
  ;; Copy data into the buffer so that it can be dup!'d. The sink does
  ;; not receive this data.
  (define (sliding-buffer-init! buffer bv)
    (let ((data (sliding-buffer-data buffer))
          (len (bytevector-length bv)))
      (bytevector-copy! bv 0 data 0 len)
      (sliding-buffer-pos-set! buffer len)))

  (define (%sliding-buffer-drain buffer pos fill)
    (let ((sink (sliding-buffer-sink buffer))
          (size (sliding-buffer-size buffer))
          (data (sliding-buffer-data buffer)))
      (let loop ((i (fxmod (fx- pos fill) size))
                 (fill fill))
        (when (fx>? fill 0)
          (let ((count (fxmin fill (fx- size i))))
            (sink data i count)
            (loop (fxmod (fx+ i count) size)
                  (fx- fill count)))))))
  
  (define (sliding-buffer-drain! buffer)
    (%sliding-buffer-drain buffer
                           (sliding-buffer-pos buffer)
                           (sliding-buffer-fill buffer))
    (sliding-buffer-fill-set! buffer 0))
  
  (define (sliding-buffer-read! buffer in-port len)
    (let ((size (sliding-buffer-size buffer))
          (data (sliding-buffer-data buffer)))
      (let loop ((pos (sliding-buffer-pos buffer))
                 (fill (sliding-buffer-fill buffer))
                 (n-left len))
        (cond ((fx=? 0 n-left)
               (sliding-buffer-pos-set! buffer pos)
               (sliding-buffer-fill-set! buffer fill)
               len)
              ((fx=? fill size)
               (%sliding-buffer-drain buffer pos fill)
               (loop pos 0 n-left))
              (else
               (let ((count (fxmin (fx- size fill) (fx- size pos) n-left)))
                 (let ((n-read (get-bytevector-n! in-port data pos count)))
                   (cond ((eof-object? n-read)
                          (sliding-buffer-pos-set! buffer pos)
                          (sliding-buffer-fill-set! buffer fill)
                          (if (fx=? n-left len)
                              (eof-object)
                              (- len n-left)))
                         (else
                          (loop (fxmod (fx+ pos n-read) size)
                                (fx+ fill n-read)
                                (fx- n-left n-read)))))))))))

  (define (sliding-buffer-put-u8! buffer u8)
    (let ((size (sliding-buffer-size buffer)))
      (when (fx=? (sliding-buffer-fill buffer) size)
        (sliding-buffer-drain! buffer))
      (let ((pos (sliding-buffer-pos buffer))
            (data (sliding-buffer-data buffer)))
        (bytevector-u8-set! (sliding-buffer-data buffer) pos u8)
        (sliding-buffer-pos-set! buffer (fxmod (fx+ pos 1) size))
        (sliding-buffer-fill-set! buffer (fx+ (sliding-buffer-fill buffer) 1)))))

  (define (sliding-buffer-dup! buffer distance len)
    (let ((size (sliding-buffer-size buffer))
          (data (sliding-buffer-data buffer)))
      (assert (< 0 distance (fx+ size 1)))
      (cond ((< distance len)
             (sliding-buffer-dup! buffer distance distance)
             (sliding-buffer-dup! buffer distance (fx- len distance)))
            (else
             (let loop ((i (mod (fx- (sliding-buffer-pos buffer) distance) size))
                        (pos (sliding-buffer-pos buffer))
                        (fill (sliding-buffer-fill buffer))
                        (n-left len))
               (cond ((fx=? 0 n-left)
                      (sliding-buffer-pos-set! buffer pos)
                      (sliding-buffer-fill-set! buffer fill))
                     ((fx=? fill size)
                      (%sliding-buffer-drain buffer pos fill)
                      (loop i pos 0 n-left))
                     (else
                      (let ((count (fxmin (fx- size i) (fx- size fill) n-left
                                          (fx- size pos))))
                        (bytevector-copy! data i data pos count)
                        (loop (fxmod (fx+ i count) size)
                              (fxmod (fx+ pos count) size)
                              (fx+ fill count)
                              (fx- n-left count)))))))))))
