;; Copyright (c) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

#!r6rs

(library (guildhall ext irregex)
  (export
    irregex
    string->irregex
    sre->irregex
    
    string->sre
    maybe-string->sre
    
    irregex?
    irregex-search
    irregex-match
    irregex-match-data?
    irregex-match-substring
    irregex-match-start-index
    irregex-match-end-index
    irregex-match-subchunk

    irregex-replace
    irregex-replace/all
    irregex-split
    irregex-extract
    irregex-fold

    make-irregex-chunker
    irregex-search/chunked
    irregex-match/chunked
    irregex-fold/chunked

    irregex-quote
    irregex-opt
    sre->string)
  (import
    (rename (except (rnrs) error remove)
            (exists any) (for-all every) (remp remove))
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (only (guile) include-from-path))
  

  (define (error . args)
    (apply assertion-violation "irregex" args))

  (include-from-path "guildhall/ext/irregex/private/irregex-r6rs")
  (include-from-path "guildhall/ext/irregex/private/irregex-utils"))
