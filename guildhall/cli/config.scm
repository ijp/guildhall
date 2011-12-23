;;; guild.scm --- Command-line UI library

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

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
(define-module (guildhall cli config)
  #:use-module (rnrs)
  #:use-module (guildhall ext fmt)
  #:use-module (guildhall ext foof-loop)
  #:use-module (guildhall spells pathname)
  #:use-module (guildhall spells filesys)
  #:use-module (guildhall private utils)
  #:use-module (guildhall config)
  #:use-module (guildhall repository)
  #:use-module (guildhall cli)
  #:export (call-with-parsed-options/config))

;; This should be different on non-POSIX systems, I guess
(define (default-config-location)
  (home-pathname '((".config" "guildhall") "config.scm")))

(define (read-config/guard pathname)
  (guard (c ((i/o-file-does-not-exist-error? c)
             (fatal (cat "specified config file `"
                         (dsp-pathname pathname) "' does not exist."))))
    (call-with-input-file (->namestring pathname)
      read-config)))

(define (call-with-parsed-options/config mod cmd-line options proc)
  (define config (default-config-location))
  (define prefix #f)
  (define config-options
    (list
     (make-option/arg '("config" #\c)
                      (lambda (val) (set! config val)))
     (make-option '("no-config")
                  (lambda () (set! config #f)))
     (make-option/arg '("prefix")
                      (lambda (arg)
                        (set! prefix arg)))))
  
  (call-with-parsed-options mod cmd-line (append options config-options)
    (lambda (args)
      (proc args
            (let ((config (if config
                              (read-config/guard config)
                              (default-config))))
              (if prefix
                  (make-prefix-config
                   prefix
                   (config-item-repositories (config-default-item config)))
                  config))))))

;; Local Variables:
;; scheme-indent-styles: ((call-with-parsed-options/config 3))
;; End:
