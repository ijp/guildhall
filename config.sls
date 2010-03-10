;;; config.sls --- Representation of configuration file data

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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
#!r6rs

(library (dorodango config)
  (export config?
          make-config
          make-prefix-config
          
          read-config
          default-config
          
          config-ref
          (rename (public:config-items config-items))
          config-default-name
          config-default-item

          config-item?
          config-item-destination
          config-item-database-location
          config-item-repositories
          config-item-cache-directory
          config-default-implementation)
  (import (rnrs)
          (only (srfi :1) append-reverse)
          (srfi :8 receive)
          (only (srfi :13) string-prefix?)
          (srfi :98 os-environment-variables)
          (only (spells misc) and=>)
          (spells alist)
          (spells match)
          (spells foof-loop)
          (spells record-types)
          (spells pathname)
          (spells tracing)
          (dorodango private utils)
          (dorodango destination)
          (dorodango repository))

(define-record-type (config %make-config config?)
  (fields items default-implementation))

(define-record-type config-item
  (fields destination database-location repositories cache-directory))

(define-functional-fields config-item
  destination database-location repositories cache-directory)

(define (make-config default
                     destinations
                     database-locations
                     repositories
                     implementation)
  (define who 'make-config)
  (loop continue ((for destination (in-list destinations))
                  (with items '()))
    => (%make-config/validate who
                              default
                              items
                              repositories
                              implementation)
    (let ((name (destination-name destination)))
      (cond ((assq-ref database-locations name)
             => (lambda (location)
                  (continue
                   (=> items
                       (cons (cons name (make-config-item destination location '() #f))
                             items)))))
            (else
             (lose who "no database location for destination" name))))))

(define (%make-config/validate who
                               default
                               items
                               repositories
                               implementation)
  (define (complete-items items)
    (map (lambda (name.item)
           (let ((new-item
                  (receive (destination
                            database-location
                            old-repositories
                            old-cache-dir)
                           (config-item-components (cdr name.item))
                    (make-config-item destination
                                      database-location
                                      (if (null? old-repositories)
                                          repositories
                                          old-repositories)
                                      (or old-cache-dir (default-cache-dir))))))
             (cons (car name.item) new-item)))
         items))
  (cond ((null? items)
         (lose who "no destinations defined"))
        (default
         (loop continue ((for name.item remaining (in-list items))
                         (for processed (listing-reverse name.item)))
           => (lose who "default destination undefined" default)
           (if (eq? default (car name.item))
               (%make-config (complete-items
                              (cons name.item
                                    (append-reverse (reverse processed)
                                                    (cdr remaining))))
                             implementation)
               (continue))))
        (else
         (%make-config (complete-items items) implementation))))

(define (public:config-items config)
  (map cdr (config-items config)))

(define (config-ref config name)
  (assq-ref (config-items config) name))

(define (config-default-item config)
  (cdar (config-items config)))

(define (config-default-name config)
  (caar (config-items config)))

(define supported-repository-types
  (list
   (lambda (name uri-string)
     (and (string-prefix? "file://" uri-string)
          (make-file-repository
           name
           (substring uri-string 7 (string-length uri-string)))))
   (lambda (name uri-string)
     (and (string-prefix? "http://" uri-string)
          (make-http-repository name uri-string)))))

(define (default-config)
  (make-prefix-config (default-prefix) (list) (default-implementation)))

(define (default-prefix)
  (home-pathname ".local"))

(define (default-implementation)
  'ikarus)

(define (default-cache-dir)
  (home-pathname '((".cache" "dorodango"))))

(define (make-prefix-config prefix repositories implementation)
  (make-config 'default
               (list (make-fhs-destination 'default prefix))
               `((default  . ,(default-db-location prefix 'default)))
               repositories
               implementation))

(define (default-db-location prefix destination-name)
  (pathname-join
   (pathname-as-directory prefix)
   `(("var" "lib" "dorodango" ,(symbol->string destination-name)))))

(define (read-config port)
  (define who 'read-config)
  (loop continue ((for form (in-port port read))
                  (with default #f)
                  (with items '())
                  (with repositories '())
                  (with implementation #f))
    => (%make-config/validate who
                              default
                              (if (null? items)
                                  (config-items (default-config))
                                  (reverse items))
                              (reverse repositories)
                              (or implementation (default-implementation)))
    (define (handle-destination+iterate name spec db-location)
      (continue
       (=> items
           (cons (cons name (make-config-item
                             (dest-spec->destination who name spec)
                             db-location
                             '()
                             #f))
                 items))))
    (match form
      (('default-destination (? symbol? name))
       (continue (=> default name)))
      (('default-implementation (? symbol? name))
       (continue (=> implementation name)))
      (('destination (? symbol? name) dest-spec ('database location))
       (handle-destination+iterate name dest-spec location))
      (('destination (? symbol? name) dest-spec)
       (handle-destination+iterate name
                                   dest-spec
                                   (default-db-location (default-prefix)
                                                        name)))
      (('repository (? symbol? name) (? string? uri))
       (loop next-type ((for constructor (in-list supported-repository-types)))
         => (lose who "unsupported repository URI" uri)
         (cond ((constructor name uri)
                => (lambda (repo)
                     (continue (=> repositories (cons repo repositories)))))
               (else
                (next-type)))))
      (else
       (lose who "invalid configuration form" form)))))

(define destination-kinds
  `((fhs . ,make-fhs-destination)))

(define (dest-spec->destination who name spec)
  (match spec
    (((? symbol? kind) . construct-args)
     (cond ((assq-ref destination-kinds kind)
            => (lambda (constructor)
                 (apply constructor name construct-args)))
           (else
            (lose who "unkown destination kind" kind))))
    (else
     (lose who "invalid destination specification" spec))))

(define-condition-type &config &error
  make-config-error config-error?)

(define (lose who message . irritants)
  (raise (condition (make-config-error)
                    (make-message-condition message)
                    (make-irritants-condition irritants))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
