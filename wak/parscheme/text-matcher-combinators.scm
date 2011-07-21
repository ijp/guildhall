;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs
(library (wak parscheme text-matcher-combinators)
  (export
    match-string
    match-string?
    matcher:char
    matcher:char=
    matcher:char/=
    matcher:char-ci=
    matcher:char-ci/=
    matcher:char-in-set
    matcher:char-not-in-set
    )
  (import (rnrs base)
          (rnrs unicode)
          (srfi :14 char-sets)
          (srfi :45 lazy)
          (only (guile) include-from-path)
          (wak riastreams)
          (wak parscheme matcher-combinators))

  (include-from-path "wak/parscheme/private/mattext"))

;;(put 'stream-lambda 'scheme-indentation 1)
