#!r6rs
;;; foreign.sls --- Foreign function interface.

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Foreign function interface allowing Scheme code to interact with
;; code written in C.
(library (spells foreign)
  (export c-type-sizeof c-type-alignof c-type-align

          make-pointer-c-getter make-pointer-c-setter
          make-pointer-c-element-getter
          make-pointer-c-element-setter

          pointer?
          null-pointer
          null-pointer?
          pointer=?
          pointer+
          let*-pointers

          make-c-callout make-c-callback

          malloc free memcpy memset

          errno

          dlopen dlsym dlclose dlerror

          pointer-short-ref
          pointer-short-set!
          pointer-ushort-ref
          pointer-ushort-set!
          pointer-int-ref
          pointer-int-set!
          pointer-long-ref
          pointer-long-set!
          pointer-ulong-ref
          pointer-ulong-set!
          pointer-llong-set!
          pointer-llong-ref
          pointer-ullong-set!
          pointer-ullong-ref

          pointer-int8-ref
          pointer-int8-set!
          pointer-uint8-ref
          pointer-uint8-set!
          pointer-uint16-ref
          pointer-uint16-set!
          pointer-uint32-ref
          pointer-uint32-set!
          pointer-uint64-ref
          pointer-uint64-set!

          pointer-ptr-ref
          pointer-ptr-set!

          utf8z-ptr->string
          string->utf8z-ptr
          ->utf8z-ptr/null

          pointer-utf8z-ptr-set!
          pointer-utf8z-ptr-ref

          define-c-callouts)
  (import (rnrs)
          (for (srfi :8 receive) run expand)
          (for (only (srfi :1) append-reverse) expand)
          (for (wak foof-loop) expand)
          (spells foreign compat)
          (spells foreign frozen-bytes)
          (spells foreign config))

  ;;@extractors (import (spells private stexidoc)) foreign-extractors


  ;;;@subheading C datatypes

  ;; Throughout this library, there various occasions where C types
  ;; must be specified. These are represented as Scheme symbols, each
  ;; corresponding to a C datatype:
  ;;@deftp {Foreign Type} short
  ;;@deftpx {Foreign Type} int
  ;;@deftpx {Foreign Type} uint
  ;;@deftpx {Foreign Type} long
  ;;@deftpx {Foreign Type} ulong
  ;;@deftpx {Foreign Type} llong
  ;;@deftpx {Foreign Type} ullong
  ;; These are the native C integer types according to the C ABI
  ;; supported by the Scheme implementation. The prefixes @code{u} and
  ;; @code{l} stand for @code{unsigned} and @code{long}, respectively;
  ;; so @code{ullong} corresponds to the C type @code{unsigned long
  ;; long}, for example.
  ;;@end deftp
  ;;
  ;;@deftp {Foreign Type} int8
  ;;@deftpx {Foreign Type} uint8
  ;;@deftpx {Foreign Type} uint16
  ;;@deftpx {Foreign Type} uint32
  ;;@deftpx {Foreign Type} uint64
  ;; The fixed-size integer types from @file{stdint.h} header.
  ;;@end deftp
  ;;
  ;;@deftp {Foreign Type} size_t
  ;;@deftpx {Foreign Type} ssize_t
  ;;@deftpx {Foreign Type} time_t
  ;; Standard types from @file{stddef.h} and @file{time.h}.
  ;;@end deftp
  ;;
  ;;@deftp {Foreign Type} pointer
  ;; Corresponds to C's @code{void *}.
  ;;@end deftp
  ;;
  ;;@deftp {Foreign Type} float
  ;;@deftpx {Foreign Type} double
  ;; C floating point types.
  ;;@end deftp

  ;;;@subsubheading About the data types

  ;; All the above C data types map naturally to Scheme numbers,
  ;; except for @code{pointer}. C pointers are opaque objects for
  ;; Scheme code, and can thus only be manipulated with the primitives
  ;; from this library.
  ;;
  ;;@defun c-type-sizeof type
  ;; Returns the size in bytes needed to store @var{type}, according
  ;; to the implementation's C ABI. This corresponds C's @code{sizeof}
  ;; operator.
  ;;@end defun
  ;;
  ;;@defun c-type-alignof type
  ;; Returns the natural alignment of @var{type}.
  ;;@end defun
  ;;
  ;;@ Returns the nearest integer greater than @var{n} which is a
  ;; multiple of the alignment of @var{ctype}. @var{n} must be an
  ;; integer, and @var{ctype} must be a symbol referring to a C type.
  (define (c-type-align ctype n)
    (let ((alignment (c-type-alignof ctype)))
      (+ n (mod (- alignment (mod n alignment)) alignment))))
  
  ;;;@subheading Basic pointer primitives
  ;;
  ;;@defun pointer? thing
  ;; Returns @code{#t} when @var{thing} can be used as a pointer. Note
  ;; that pointers are not guaranteed to be of a disjoint type -- an
  ;; implementation may for example use integers to represent C pointers.
  ;;@end defun
  ;;
  ;;@defun null-pointer? thing
  ;; Returns @code{#t} when @var{thing} represents the NULL pointer.
  ;;@end defun
  ;;
  ;;@defun pointer=? pointer-1 pointer-2
  ;; Compares @var{pointer-1} and @var{pointer-2}, which both must
  ;; satisfy @ref{pointer?} for equality.
  ;;@end defun
  ;;
  ;;@defun null-pointer
  ;; Returns the object representing the NULL pointer.
  ;;@end defun
  ;;
  ;;@defun pointer+ pointer offset
  ;; Returns the pointer resulting from adding the integer
  ;; @var{offset} to @var{pointer}.
  ;;@end defun
  
  
  ;;;@subheading Calling out to C

  ;; To be able to call out to C a handle to a @emph{shared object}
  ;; has to be obtained first.
  ;;
  ;;@defun dlopen name
  ;; Returns a handle for the shared object referred to by the
  ;; string @var{name}. How @var{name} is mapped to the shared object
  ;; on the filesystem is platform-dependent. On error, @code{#f} is
  ;; returned; the error message can be obtained with @ref{dlerror}.
  ;;@end defun
  ;;
  ;;@defun dlclose shared-object
  ;; Free the resources associated with @var{shared-object}. Note that on
  ;; some Scheme implementations, this may not actually be possible. In
  ;; that case, the procedure does nothing.
  ;;@end defun
  ;;
  ;;@defun dlsym shared-object name
  ;; Lookup the C function @var{name} in @var{shared-object}. On success,
  ;; a pointer that can be used with @ref{make-c-callout} is returned.
  ;; When the named C function does not exist the return value is @code{#f}.
  ;;@end defun
  ;;
  ;;@defun dlerror
  ;; Returns the error message resulting from a failed call
  ;; to @ref{dlopen}.
  ;;@end defun
  ;;
  ;; The primitive procedure for creating callout wrappers is
  ;; @ref{make-c-callout}, but if the function signature and shared
  ;; object the C function resides in are known before run-time, it
  ;; may be more convinient to use the convinience macro
  ;; @ref{define-c-callouts}.
  ;;
  ;;@defun make-c-callout return-type argument-types
  ;;
  ;; Returns a callout procedure generator, which is a single-argument
  ;; procedure which, when called with a pointer retrieved by
  ;; @ref{dlsym}, returns a procedure that wraps around the C function
  ;; found by @code{dlsym}. The signature of the C functions that
  ;; Scheme wrappers will be created for is described by
  ;; @var{return-type}, which must be a symbol referring to a C type,
  ;; and @var{argument-types}, which must be a list of such
  ;; symbols.
  ;;
  ;; Note that each invocation of the callout procedure generator may,
  ;; depending upon the Scheme implementation, leak a small amount of
  ;; memory.
  ;;
  ;;@end defun
  ;;
  ;;@defspec define-c-callouts shared-object @var{binding} ...
  ;; Define callout procedures into a shared object.
  ;;
  ;; Each @var{binding} for defines a callout procedure that wraps
  ;; around a C function residing in @var{shared-object}, and has the
  ;; following form:
  ;;
  ;; @code{(@var{name} @var{return-type} @var{c-name} @var{argument-types})}
  ;;
  ;; The created callout will be bound to the identifier @var{name},
  ;; and correspond to the C function referred to by @var{c-name},
  ;; which must evaluate to a string. The signature of the C function
  ;; is specified by @var{return-type} and @var{argument-types}, where
  ;; the former must evaluate to symbol denoting a primitive C type,
  ;; and the latter must evaluate to a list of C type symbols.
  ;;
  ;;@end defspec
  
  (define-syntax define-c-callouts
    ;;++ One could implement callout compression (i.e. reusing
    ;; callouts with the same signature)
    (syntax-rules ()
      ((define-callouts shared-object (name ret-type c-name arg-types) ...)
       (begin
         (define name ((make-c-callout ret-type arg-types)
                       (or (dlsym shared-object c-name)
                           (error 'define-c-callouts
                                  "symbol lookup in shared object failed"
                                  shared-object
                                  c-name))))
         ...))))

  ;;;@subheading Calling back to Scheme

  ;; When control has entered C code, it is sometimes required that
  ;; the C code is able to call back into Scheme. This is accomplished
  ;; with the following procedure:
  ;;
  ;;@defun make-c-callback return-type argument-types
  ;; Returns a callback pointer generator, which is a single-argument
  ;; procedure which accepts a Scheme procedure and returns a C
  ;; pointer. That C pointer can be used as a function pointer from C,
  ;; provided that the C type signature used for invoking the function
  ;; pointer matches @var{return-type} and
  ;; @var{argument-types}. Invocation of the function pointer will
  ;; cause the Scheme procedure provided as argument to
  ;; @code{make-c-callback} to be invoked with arguments corresponding
  ;; to @var{argument-types}.
  ;;
  ;; Note that each invocation of the callback pointer generator may,
  ;; depending upon the Scheme implementation, leak a small amount of
  ;; memory.
  ;;@end defun

  
  ;;;@subheading Memory allocation

  ;;@defun malloc size
  ;; Returns a pointer to a memory region of @var{size} bytes
  ;; allocated by C's @code{malloc} function.
  ;;@end defun

  ;;@defun free pointer
  ;; Free a pointer obtained from @ref{malloc} or known to be
  ;; allocated by @code{malloc} from C.
  ;;@end defun
  
  
  ;;;@subheading Dealing with C strings
  
  ;;@ This function takes a pointer to NUL-terminated UTF-8 data and
  ;; returns the corresponding Scheme string. It, like
  ;; @uref{http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_idx_194,
  ;; R6RS utf8->string} uses replacement replacement semantics for
  ;; invalid UTF-8 sequences.
  (define (utf8z-ptr->string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-uint8-ref ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))

  ;;@ This function converts a string to @ref{malloc}'ed block of
  ;; zero-terminated UTF-8 data.
  (define (string->utf8z-ptr s)
    (let* ((bytes (string->utf8 s))
           (bytes-len (bytevector-length bytes))
           (result (malloc (+ bytes-len 1))))
      (memcpy result bytes bytes-len)
      (pointer-uint8-set! result bytes-len 0)
      result))

  ;;@ This function is like @ref{string->utf8z-ptr}, but it
  ;; additionally accepts @code{#f}, and returns a NULL pointer in that case.
  (define (->utf8z-ptr/null s)
    (cond ((string? s) (string->utf8z-ptr s))
          ((eqv? s #f)
           (null-pointer))
          (else
           (assertion-violation '->utf8z-ptr/null "invalid argument type" s))))

  ;;@ Set an element in a C array of character pointers. The procedure
  ;; will store the result of converting the string @var{value} at
  ;; @var{offset} bytes from @var{ptr} to the result of converting the
  ;; string @var{value} to a pointer with
  ;; @ref{string->utf8z-ptr}. Additionally, if @var{value} is a
  ;; pointer, this procedure will act identically as
  ;; @ref{pointer-ptr-set!}.
  (define (pointer-utf8z-ptr-set! ptr offset value)
    (pointer-ptr-set! ptr offset (if (pointer? value)
                                     value
                                     (string->utf8z-ptr value))))

  ;;@ Retrieve an element in a C array of pointers to UTF-8 encoded
  ;; character data. This procedure returns the string corresponding
  ;; to the character pointer at @var{offset} bytes in the array
  ;; pointed to by @var{ptr}, unless that element is a NULL pointer,
  ;; in which case it returns @code{#f}.
  (define (pointer-utf8z-ptr-ref ptr offset)
    (let ((utf8z-ptr (pointer-ptr-ref ptr offset)))
      (if (null-pointer? utf8z-ptr)
          #f
          (utf8z-ptr->string utf8z-ptr))))

  ;;;@subheading Accessing memory
  
  ;;@ These procedures return the C value (of the type indicated by
  ;; their name) located at @var{offset} bytes from the memory
  ;; location @var{pointer}.
  (define pointer-short-ref   (make-pointer-c-getter 'short))
  (define pointer-ushort-ref  (make-pointer-c-getter 'ushort))
  (define pointer-int-ref     (make-pointer-c-getter 'int))
  (define pointer-uint-ref    (make-pointer-c-getter 'uint))
  (define pointer-long-ref    (make-pointer-c-getter 'long))
  (define pointer-ulong-ref   (make-pointer-c-getter 'ulong))
  (define pointer-llong-ref   (make-pointer-c-getter 'llong))
  (define pointer-ullong-ref  (make-pointer-c-getter 'ullong))
  (define pointer-int8-ref    (make-pointer-c-getter 'int8))
  (define pointer-uint8-ref   (make-pointer-c-getter 'uint8))
  (define pointer-uint16-ref  (make-pointer-c-getter 'uint16))
  (define pointer-uint32-ref  (make-pointer-c-getter 'uint32))
  (define pointer-uint64-ref  (make-pointer-c-getter 'uint64))
  (define pointer-ptr-ref     (make-pointer-c-getter 'pointer))

  ;;@ These procedures store @var{value} as the C type
  ;; indicated by their name at @var{offset} bytes from the memory
  ;; location @var{pointer}.  
  (define pointer-short-set!  (make-pointer-c-setter 'short))
  (define pointer-ushort-set! (make-pointer-c-setter 'ushort))
  (define pointer-int-set!    (make-pointer-c-setter 'int))
  (define pointer-uint-set!   (make-pointer-c-setter 'uint))
  (define pointer-long-set!   (make-pointer-c-setter 'long))
  (define pointer-ulong-set!  (make-pointer-c-setter 'ulong))
  (define pointer-llong-set!  (make-pointer-c-setter 'llong))
  (define pointer-ullong-set! (make-pointer-c-setter 'ullong))
  (define pointer-int8-set!   (make-pointer-c-setter 'int8))
  (define pointer-uint8-set!  (make-pointer-c-setter 'uint8))
  (define pointer-uint16-set! (make-pointer-c-setter 'uint16))
  (define pointer-uint32-set! (make-pointer-c-setter 'uint32))
  (define pointer-uint64-set! (make-pointer-c-setter 'uint64))
  (define pointer-ptr-set!    (make-pointer-c-setter 'pointer))

  ;;@ Returns a procedure that can be used to retrieve a value from a
  ;; C struct. The returned retrieval procedure expects a single
  ;; pointer argument. The element to be fetched is expected to be of
  ;; the C type @var{type} and located at @var{offset} bytes in the
  ;; struct. If @var{bit-offset} and @var{bits} are not @code{#f}, the
  ;; retrieval procedure returns only the bits of the C struct value
  ;; indicated by these parameters.
  (define (make-pointer-c-element-getter type offset bit-offset bits)
    (case type
      ((record union array)
         (lambda (pointer)
           (pointer+ pointer offset)))
      (else
       (let ((ptr-ref (make-pointer-c-getter type)))
         (cond ((and bits bit-offset)
                (let ((end-offset (+ bit-offset bits)))
                  (lambda (pointer)
                    (let ((val (ptr-ref pointer offset)))
                      (bitwise-bit-field val bit-offset end-offset)))))
               (else
                (lambda (pointer) (ptr-ref pointer offset))))))))

  ;;@ Returns a procedure that can be used to store a value inside a C
  ;; struct. The returned procedure expects two arguments, a pointer
  ;; and a value. The type and offset inside the struct are specified
  ;; by @var{type} and @var{offset}, respectively. If @var{bit-offset}
  ;; and @var{bits} are not @code{#f}, the value to be stored (which
  ;; must be an integer in this case), will be stored at the indicated
  ;; bit position inside the C struct value at @var{offset}.
  (define (make-pointer-c-element-setter type offset bit-offset bits)
    (define (lose msg . irritants)
      (apply error 'make-pointer-c-element-setter msg irritants))
    (case type
      ((record union array)
       (lose "cannot set compound element" type))
      (else
       (let ((ptr-set (make-pointer-c-setter type)))
         (cond ((and bits bit-offset)
                (let ((end-offset (+ bit-offset bits))
                      (ptr-ref (make-pointer-c-getter type))
                      (mask (bitwise-not (bitwise-arithmetic-shift-left -1 bit-offset))))
                  (lambda (pointer val)
                    (let ((val (ptr-ref pointer offset)))
                      (ptr-set pointer offset
                               (bitwise-copy-bit-field
                                val bit-offset end-offset
                                (bitwise-arithmetic-shift-left
                                 (bitwise-and val mask)
                                 bit-offset)))))))
               (else
                (lambda (pointer val) (ptr-set pointer offset val))))))))

  ;;@defun memcpy target source count
  ;;@defunx memcpy target target-start source source-start count
  ;;
  ;; Transfer @var{count} bytes between Scheme and C. If @var{target}
  ;; is a bytevector, @var{source} must be pointer, and vice
  ;; versa. The second variant of this function allows to specify
  ;; offsets (in bytes) into the memory areas. Both @var{source-start}
  ;; and @var{target-start} default to 0. Returns @var{target}.
  ;;
  ;;@end defun

  ;;@defun memset pointer value count
  ;; Set @var{count} bytes at @var{pointer} to @var{value}, which must
  ;; be an integer between 0 and 255.
  ;;@end defun

  ;;@defspec let*-pointers (@var{binding} ...) @var{body} ...
  ;;
  ;; Establish pointer bindings for callouts. Each @var{binding} can
  ;; be one of the following forms:
  ;;
  ;; @table @samp
  ;;
  ;; @item (@var{id} @var{cleanup-proc} @var{ptr-expr})
  ;;
  ;; Binds @var{id} to @var{ptr-expr}, which must evaluate to a
  ;; pointer. After @var{body} has been executed, @var{cleanup-proc},
  ;; which must evaluate to a procedure, is applied to @var{id}.
  ;;
  ;; @item (@var{id} <= @var{bv-expr})
  ;; @item (@var{id} <= @var{bv-expr} @var{start})
  ;; @item (@var{id} <= @var{bv-expr} @var{start} @var{end})
  ;;
  ;; Binds @var{id} to a pointer into @var{bv-expr}, which must
  ;; evaluate to a bytevector. The memory area pointed to by @var{id}
  ;; starts at offset @var{start} within the bytevecor and ends at
  ;; @var{end}. @var{start} and @var{end} default to 0 and the length
  ;; of the bytevector, respectively. The code in @var{body} may read
  ;; from @var{id}, and will see the contents of the bytevector, but
  ;; any modification done by @var{body} is not guaranteed to be
  ;; reflected in the bytevector.
  ;;
  ;; @item (@var{id} => @var{bv-expr})
  ;; @item (@var{id} => @var{bv-expr} @var{start})
  ;; @item (@var{id} => @var{bv-expr} @var{start} @var{end})
  ;;
  ;; This set of clauses work like the previous set, but the intent is
  ;; different. Use one of these clauses when @var{body} modifies the
  ;; memory at @var{id}, but does not read from it.
  ;;
  ;; @end table
  ;;
  ;;@end defspec
  (define-syntax let*-pointers
    (lambda (stx)
      (define (process-bindings bindings)
        (loop continue ((with bds '())
                        (with cleanup-actions '())
                        (for binding (in-list bindings)))
          => (values (reverse bds)
                     cleanup-actions)
          (define (frozen-bytes direction id expr args)
            (with-syntax (((fbytes) (generate-temporaries '(frozen-bytes))))
              (continue (=> bds
                            (append-reverse
                             (list #`(fbytes
                                      (freeze-bytes '#,direction #,expr #,@args))
                                   #`(#,id (frozen-bytes-pointer fbytes)))
                             bds))
                        (=> cleanup-actions
                            (cons #'(unfreeze-bytes fbytes)
                                  cleanup-actions)))))
          (syntax-case binding (=> <=)
            ;; input data
            ((id <= expr arg ...)
             (frozen-bytes #'in #'id #'expr #'(arg ...)))
            ((id => expr arg ...)
             (frozen-bytes #'out #'id #'expr #'(arg ...)))
            ((id free expr)
             (continue (=> bds (cons #'(id expr) bds))
                       (=> cleanup-actions
                           (cons #'(free expr) cleanup-actions)))))))
      (syntax-case stx ()
        ((_ (binding ...) body0 body ...)
         (receive (bds cleanup-actions)
                  (process-bindings #'(binding ...))
           #`(let* #,bds
               (receive results (begin body0 body ...)
                 #,@cleanup-actions
                 (apply values results))))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
