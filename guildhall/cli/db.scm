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
(define-module (guildhall cli db)
  #:use-module (rnrs)
  #:use-module (guildhall ext fmt)
  #:use-module (guildhall spells pathname)
  #:use-module (guildhall spells filesys)
  #:use-module (guildhall spells logging)
  #:use-module (guildhall private utils)
  #:use-module (guildhall database)
  #:use-module (guildhall package)
  #:use-module (guildhall config)
  #:use-module (guildhall repository)
  #:use-module (guildhall ui formatters)
  #:use-module (guildhall cli)
  #:use-module (guildhall cli config)
  #:export (call-with-parsed-options/config+db))

(define* (open-database* config #:key
                         (destination (config-default-name config))
                         (repositories '()))
  (let* ((item (if destination
                   (or (config-ref config destination)
                       (fatal (cat "no such destination configured: " destination)))
                   (config-default-item config)))
         (location (config-item-database-location item)))
    (guard (c ((database-locked-error? c)
               (fatal (cat "database locked: " (dsp-pathname location)))))
      (open-database location
                     (config-item-destination item)
                     (append repositories (config-item-repositories item))
                     (config-item-cache-directory item)))))

(define (call-with-parsed-options/config+db mod cmd-line options proc)
  (define dest #f)
  (define repos '())
  (call-with-parsed-options/config
      mod cmd-line
      (append (list (make-option/arg
                     '("dest" #\d)
                     (lambda (arg)
                       (set! dest (string->symbol arg))))
                    (make-option/arg
                     '("repo" #\r)
                     (lambda (arg)
                       (set! repos
                             (append repos
                                     (list (uri-string->repository arg)))))))
              options)
    (lambda (args config)
      (call-with-database (open-database* config #:destination dest
                                          #:repositories repos)
        (lambda (db)
          (with-throw-handler #t
            (lambda ()
              (proc args config db))
            (lambda _
              (close-database db))))))))

;; Local Variables:
;; scheme-indent-styles: ((call-with-database 1) (call-with-parsed-options/config+db 3))
;; End:
