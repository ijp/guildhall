#!r6rs
;;; fmt-js.scm -- fmt-js for R6RS
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; R6RS adaption Copyright (c) 2011 Andreas Rottmann, same license.

(library (wak fmt js)
 (export js-expr js-function js-var js-comment js-array js-object js=== js>>>)
 (import (rnrs base)
         (only (guile) include-from-path)
         (wak fmt)
         (wak fmt c))
 
 (include-from-path "wak/fmt/private/fmt-js"))
