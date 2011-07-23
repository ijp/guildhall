;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2011 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; Guile-dependent hooks for (guild weinholt compression zip).  Based on the
;; hooks for Ikarus.

(library (guild weinholt compression zip extra (0 0 20100411))
  (export call-with-adorned-output-file get-file-attributes)
  (import (rnrs)
          (only (srfi :1 lists) drop-right)
          (only (srfi :13 strings) string-suffix? string-trim
                string-prefix? string-contains string-tokenize)
          (only (guile) string-split mkdir stat stat:type
                stat:mtime stat:ctime access? X_OK chmod)
          (srfi :19 time)
          (guild weinholt struct pack))

  (define os-dos 0)
  (define os-unix 3)
  ;; etc etc

  ;; TODO: Windows support. This might involve converting between /
  ;; and \?

  ;; TODO: change file times. I didn't see a procedure for that.
  
  (define (call-with-adorned-output-file inzip-filename date local-extra
                                         central-extra
                                         os-made-by
                                         internal-attributes
                                         external-attributes
                                         uncompressed-size
                                         proc)
    (cond ((or (and (> (string-length inzip-filename) 1)
                    (char=? #\: (string-ref inzip-filename 1)))
               (string-prefix? "\\" inzip-filename)
               (string-contains inzip-filename ".."))
           ;; Ypsilon runs on Windows, and this is my lame attempt at
           ;; looking for absolute filenames and drive specs. Also
           ;; looks for ../ etc. Maybe it'd be better to remove them
           ;; instead of raising this error?
           (error 'call-with-adorned-output-file
                  "I'm putting my foot down, and I will not create this file"
                  inzip-filename))
          ((and (string-suffix? "/" inzip-filename) (zero? uncompressed-size))
           ;; Directory.
           (unless (file-exists? inzip-filename)
             (mkdir inzip-filename))
           0)
          ((and (not date) (= os-dos os-made-by) (zero? uncompressed-size))
           ;; Volume label.
           0)
          (else
           ;; Create the file's directory
           (when (string-contains inzip-filename "/")
             (let ((parts (drop-right (string-split inzip-filename #\/) 1)))
               (let lp ((parts (cdr parts))
                        (dir (car parts)))
                 (unless (file-exists? dir)
                   (mkdir dir))
                 (unless (null? parts)
                   (lp (cdr parts)
                       (string-append dir "/" (car parts)))))))
           (let ((ret
                  (call-with-port (open-file-output-port inzip-filename)
                    proc)))
             (if (= os-made-by os-unix)
                 (chmod
                  inzip-filename
                  (bitwise-and
                   (bitwise-not #o022)  ;umask...?
                   (bitwise-arithmetic-shift-right external-attributes 16))))
             ret))))

  ;; This procedure will be used when creating .ZIP files. The data
  ;; types are the same as for the previous procedure, except the
  ;; filename is from the implementation's perspective. The *returned*
  ;; filename should be suitable for inclusion in the .zip file. This
  ;; means that the path separator becomes #\/ and directories have a
  ;; #\/ appended.
  (define (get-file-attributes fn)
    (let ((buf (stat fn))
          (flags (bitwise-ior #b001     ;mtime is present
                              #b100)))  ;ctime is present
      (let ((date (time-monotonic->date
                   (make-time 'time-monotonic 0 (stat:mtime buf))))
            (local (list (cons #x5455 (pack "<uCll" flags (stat:mtime buf)
                                            (stat:ctime buf)))))
            (central (list (cons #x5455 (pack "<uCl" flags (stat:mtime buf)))))
            ;; Add / to directory names
            (fn (if (and (eq? 'directory (stat:type (stat fn)))
                         (not (string-suffix? "/" fn)))
                    (string-append fn "/")
                    fn)))
        
        (values
          ;; Remove leading /
          (string-trim fn #\/)          ;filename in .zip file
          date                          ;date
          local                         ;local-extra
          central                       ;central-extra
          os-unix                       ;os-made-by
          0                             ;internal-attributes
          ;; External attributes
          (bitwise-arithmetic-shift-left
           (if (or (eq? 'directory (stat:type (stat fn)))
                   (access? fn X_OK))
               #o755
               #o644)
           16)))))

  )
