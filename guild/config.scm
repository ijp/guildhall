;;; config.scm --- Representation of configuration file data

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (guild config)
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
          config-item-cache-directory)
  (import (rnrs)
          (only (srfi :1) append-reverse)
          (srfi :8 receive)
          (srfi :98 os-environment-variables)
          (guild ext foof-loop)
          (guild ext foof-loop nested)
          (only (guile) assq-ref and=>)
          (ice-9 match)
          (guild spells record-types)
          (guild spells pathname)
          (guild private utils)
          (guild destination)
          (guild repository))

(define-record-type (config %make-config config?)
  (fields items))

(define-record-type config-item
  (fields destination database-location repositories cache-directory))

(define-functional-fields config-item
  destination database-location repositories cache-directory)

(define (make-config default
                     destinations
                     database-locations
                     repositories)
  (define who 'make-config)
  (loop continue ((for destination (in-list destinations))
                  (with items '()))
    => (%make-config/validate who
                              default
                              items)
    (let ((name (destination-name destination)))
      (cond ((assq-ref database-locations name)
             => (lambda (location)
                  (continue
                   (=> items
                       (cons (cons name (make-config-item
                                         destination
                                         location
                                         repositories
                                         (default-cache-directory)))
                             items)))))
            (else
             (lose who "no database location for destination" name))))))

(define (%make-config/validate who
                               default
                               items)
  (cond ((null? items)
         (lose who "no destinations defined"))
        (default
         (loop continue ((for name.item remaining (in-list items))
                         (for processed (listing-reverse name.item)))
           => (lose who "default destination undefined" default)
           (if (eq? default (car name.item))
               (%make-config (cons name.item
                                   (append-reverse (reverse processed)
                                                   (cdr remaining))))
               (continue))))
        (else
         (%make-config items))))

(define (public:config-items config)
  (map cdr (config-items config)))

(define (config-ref config name)
  (assq-ref (config-items config) name))

(define (config-default-item config)
  (cdar (config-items config)))

(define (config-default-name config)
  (caar (config-items config)))


(define (default-config)
  (make-prefix-config (default-prefix) (list)))

(define (default-prefix)
  (home-pathname ".local"))

(define (default-cache-directory)
  (home-pathname '((".cache" "guild"))))

(define (make-prefix-config prefix repositories)
  (make-config 'default
               (list (make-fhs-destination 'default prefix))
               `((default  . ,(default-db-location prefix)))
               repositories))

(define (default-db-location prefix)
  (pathname-join (pathname-as-directory prefix) '(("var" "lib" "guild"))))

(define (read-config port)
  (define who 'read-config)
  (loop continue ((for form (in-port port read))
                  (with default #f)
                  (with items '())
                  (with repositories '()))
    => (if (null? items)
           (make-prefix-config (default-prefix)
                               repositories)
           (%make-config/validate who
                                  default
                                  (reverse items)))
    (define (parse-destination-options options)
      (loop continue ((for option (in-list options))
                      (with db-location #f)
                      (with configured-repos #f))
        => (values db-location
                   (and=> configured-repos reverse))
        (match option
          (('database (? string? location))
           (when db-location
             (lose who "multiple database locations configured" option))
           (continue (=> db-location location)))
          (('repositories names ___)
           (unless (for-all symbol? names)
             (lose who "repository identifiers must be symbols" option))
           (let ((repos (collect-list (for name (in-list names))
                          (or (find (lambda (repo)
                                      (eq? name (repository-name repo)))
                                    repositories)
                              (lose who "undefined repository" name)))))
             (continue (=> configured-repos
                           (append-reverse repos (or configured-repos '()))))))
          (_
           (lose who "malformed destination option" option)))))
    (match form
      (('default-destination (? symbol? name))
       (continue (=> default name)))
      (('destination (? symbol? name) dest-spec . options)
       (let-values (((db-location configured-repos)
                     (parse-destination-options options))
                    ((destination) (dest-spec->destination who name dest-spec)))
         (continue
          (=> items
              (cons
               (cons name
                     (make-config-item
                      destination
                      (or db-location
                          (default-db-location (destination-prefix destination)))
                      (or configured-repos (reverse repositories))
                      (default-cache-directory)))
               items)))))
      (('repository (? symbol? name) (? string? uri))
       (continue (=> repositories
                     (cons (or (uri-string->repository uri name)
                               (lose who "unsupported repository URI" uri))
                           repositories))))
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
