;;; pkg-list.scm --- Dorodango package list

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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


;;@ A package manager for R6RS implementations.
(package (dorodango (0))
  (depends (srfi)
           (wak-foof-loop)
           (wak-fmt)
           (wak-irregex)
           (wak-parscheme)
           (spells)
           (industria)
           (ocelotl))
  (libraries (sls -> "dorodango")
             ("private" -> ("dorodango" "private")))
  (programs (("scripts" "doro.sps") -> "doro"))
  (installation-hook ()
    (import (except (rnrs) file-exists? delete-file)
            (srfi :8 receive)
            (wak fmt)
            (spells pathname)
            (spells filesys)
            (dorodango ui cmdline)
            (dorodango ui cmdline help))
    (lambda (agent)
      (receive (pathname port) (create-temp-file)
        (call-with-port port
          (lambda (port)
            (fmt port (dsp-man-page (command-list)))))
        (agent 'install-file 'man "doro.1" (->namestring pathname))
        (delete-file pathname)))))

;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
