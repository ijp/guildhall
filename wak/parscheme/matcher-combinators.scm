;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs
(library (wak parscheme matcher-combinators)
  (export
    match
    define-matcher

    ;; Matcher combinators
    matcher:at-least
    matcher:at-least-until
    matcher:at-most
    matcher:at-most-until
    matcher:between
    matcher:between-until
    matcher:bracketed
    matcher:bracketed*
    matcher:choice
    matcher:comparison
    matcher:deep-choice
    matcher:end
    matcher:epsilon
    matcher:error
    matcher:exactly
    matcher:if
    matcher:left-comparison
    matcher:optional
    matcher:peek
    matcher:repeated
    matcher:repeated-until
    matcher:right-comparison
    matcher:sequence
    matcher:token
    matcher:token-if

    ;; Higher-order matcher combinators
    comparator-matcher
    left-comparator-matcher
    right-comparator-matcher
    guarded-matcher
    )
  (import (except (rnrs base) error)
          (rnrs io simple)
          (except (srfi :1 lists) for-each map)
          (srfi :45 lazy)
          (ice-9 streams)
          (wak private include))

  (include-file/downcase ((wak parscheme private) matcomb)))
