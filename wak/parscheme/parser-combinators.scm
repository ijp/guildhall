;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

(library (wak parscheme parser-combinators)
  (export
    parse-stream
    define-parser
    *parser
    parser:at-least
    parser:at-least-until
    parser:at-most
    parser:at-most-until
    parser:backtrackable
    parser:between
    parser:between-until
    parser:bracketed
    parser:bracketed*
    parser:bracketed-noise
    parser:bracketed-list
    parser:call-with-context
    parser:choice
    parser:complete
    parser:context
    parser:deep-choice
    parser:delayed
    parser:end
    parser:epsilon
    parser:error
    parser:eqv-token
    parser:exactly
    parser:extend
    parser:label
    parser:list:at-least
    parser:list:at-least-until
    parser:list:at-most
    parser:list:at-most-until
    parser:list:between
    parser:list:between-until
    parser:list:exactly
    parser:list:repeated
    parser:list:repeated-until
    parser:map
    parser:match
    parser:match->list
    parser:match->ignore
    parser:modify-context
    parser:noise:at-least
    parser:noise:at-least-until
    parser:noise:at-most
    parser:noise:at-most-until
    parser:noise:between
    parser:noise:between-until
    parser:noise:exactly
    parser:noise:repeated
    parser:noise:repeated-until
    parser:on-failure
    parser:optional
    parser:optional-noise
    parser:peek
    parser:refuse
    parser:repeated
    parser:repeated-until
    parser:return
    parser:sequence
    parser:set-context
    parser:token
    parser:token*
    parser:token-if
    )
  (import (except (rnrs base) error)
          (rnrs io simple)
          (except (srfi :1 lists) for-each map)
          (srfi :8 receive)
          (srfi :9 records)
          (srfi :45 lazy)
          (wak private include)
          (wak riastreams)
          (wak parscheme parse-errors)
          (wak parscheme matcher-combinators))

  (include-file/downcase ((wak parscheme private) parcomb)))
