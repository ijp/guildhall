;;; zip.sls --- zip utilities for dorodango

;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (dorodango private zip)
  (export zip-port->inventory
          extract-zip-entry)
  (import (rnrs)
          (srfi :8 receive)
          (only (srfi :13) string-null?)
          (wak foof-loop)
          (spells string-utils)
          (prefix (weinholt compression zip) zip:)
          (dorodango inventory))

;; This should be made more robust; if there are inconsistencies in
;; the central directory file names (such as duplicates or "files
;; inside a file"), `inventory-update' will error out; this should be
;; caught and appropriately reported.
(define (zip-port->inventory port)
  (let ((zip-dir (zip:get-central-directory port)))
    (loop continue ((for entry (in-list zip-dir))
                    (with inventory (make-inventory 'root #f)))
          => inventory
          (if (zip:central-directory? entry)
              (receive (path container?)
                       (filename->path (zip:central-directory-filename entry))
                (if (null? path)
                    (continue) ;bad zipfile, probably should error out here
                    (continue
                     (=> inventory
                         (inventory-leave-n
                          (inventory-update inventory path container? entry)
                          (length path))))))
              (continue)))))

(define (extract-zip-entry zip-port zip-central dest-port)
  (let ((zip-local (zip:central-directory->file-record zip-port zip-central)))
    (zip:extract-to-port zip-port zip-local zip-central dest-port)))

(define (filename->path filename)
  (let* ((path (string-split filename #\/))
         (relative-path (if (and (not (null? path))
                                 (string-null? (car path)))
                            (cdr path)
                            path))
         (reversed-path (reverse relative-path)))
    (if (and (not (null? reversed-path))
             (string-null? (car reversed-path)))
        (values (reverse (cdr reversed-path)) #t)
        (values relative-path #f))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
