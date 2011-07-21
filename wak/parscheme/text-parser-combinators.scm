;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

(library (wak parscheme text-parser-combinators)
  (export
    parse-file
    parse-string
    parse-input-chars
    parser:bracketed-string
    parser:char
    parser:char=
    parser:char/=
    parser:char-ci=
    parser:char-ci/=
    parser:char-in-set
    parser:char-not-in-set
    parser:list->string
    parser:match->string
    parser:reverse-list->string
    parser:string=
    parser:string-ci=
    parser:string:at-least
    parser:string:at-least-until
    parser:string:at-most
    parser:string:at-most-until
    parser:string:between
    parser:string:between-until
    parser:string:exactly
    parser:string:repeated
    parser:string:repeated-until
    )
  (import (except (rnrs base) error)
          (rnrs unicode)
          (rnrs io simple)
          (srfi :6 basic-string-ports)
          (srfi :14 char-sets)
          (srfi :45 lazy)
          (wak private include)
          (ice-9 streams)
          (wak parscheme parser-combinators))

  (include-file/downcase ((wak parscheme private) partext)))
