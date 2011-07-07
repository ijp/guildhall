;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

(library (wak parscheme parse-errors)
  (export
    parse-error?
    parse-error/position
    parse-error/messages
    merge-parse-errors
    parse-error-with-position
    make-parse-error
    make-parse-error:trailing-garbage
    make-parse-error:unknown
    make-parse-error:unexpected-end-of-input
    make-parse-error:unexpected-token
    )
  (import (rnrs base)
          (srfi :9 records)
          (wak private include))

  (include-file/downcase ((wak parscheme private) perror)))
