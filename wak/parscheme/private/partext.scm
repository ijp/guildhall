;;; -*- Mode: Scheme; scheme48-package: text-parser-combinators -*-

;;;; Parsing Tools
;;;; Combinators for Parsing Text

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (parse-file parser pathname context win lose)
  (call-with-input-file pathname
    (lambda (input-port)
      (parse-input-chars parser input-port context win lose))))

(define (parse-input-chars parser input-port context win lose)
  (parse-stream parser
                (let recur ()
                  (lazy (let ((char (read-char input-port)))
                          (if (eof-object? char)
                              stream-nil
                              (stream-cons char (recur))))))
                (cons 1 1)
                (lambda (position char)
                  (let ((line (car position))
                        (column (cdr position)))
                    (if (char=? char #\newline)
                        (cons (+ line 1) 1)
                        (cons line (+ column 1)))))
                context
                win
                lose))

(define (parse-string parser string context win lose)
  (parse-stream parser
                (string->stream string)
                0
                (lambda (position token) token (+ position 1))
                context
                win
                lose))

(define (parser:char)
  (parser:token-if char?))

(define (parser:char-test procedure argument)
  (parser:token-if (lambda (token)
                     (and (char? token)
                          (procedure argument token)))))

(define (char/=? a b) (not (char=? a b)))
(define (char-ci/=? a b) (not (char-ci=? a b)))

(define (parser:char= char) (parser:char-test char=? char))
(define (parser:char/= char) (parser:char-test char/=? char))
(define (parser:char-ci= char) (parser:char-test char-ci=? char))
(define (parser:char-ci/= char) (parser:char-test char-ci/=? char))

(define (parser:char-in-set char-set)
  (parser:char-test char-set-contains? char-set))

(define (parser:char-not-in-set char-set)
  (parser:char-test (lambda (char-set char)
                      (not (char-set-contains? char-set char)))
                    char-set))

(define (parser:list->string parser)
  (parser:map list->string parser))

(define (parser:reverse-list->string parser)
  (parser:map reverse-list->string parser))

(define (reverse-list->string list)     ;++ fix
  (list->string (reverse list)))

(define (parser:match->string matcher)
  ;++ This could be improved dramatically if we improved matchers.
  (parser:list->string (parser:match->list matcher)))

(define (parser:string-compare parser:char-compare string)
  (let recur ((index 0))
    (if (= index (string-length string))
        (parser:return string)
        (*parser ((parser:char-compare (string-ref string index)))
          (recur (+ index 1))))))

(define (parser:string= string)
  (parser:string-compare parser:char= string))

(define (parser:string-ci= string)
  (parser:string-compare parser:char-ci= string))

;;;; Specialized Reptitions for Strings

;;; For convenience, these just use DISPLAY to generate the output.
;;; Whether this is the right thing, I don't know.

(define (parser:get-output-string parser)
  (parser:map get-output-string parser))

(define output-string-parser (parser:epsilon open-output-string))

(define (display* object output-port)
  (display object output-port)
  output-port)

(define (parser:string:repeated parser)
  (parser:get-output-string
   (parser:repeated display* output-string-parser parser)))

(define (parser:string:repeated-until terminal-parser parser)
  (parser:get-output-string
   (parser:repeated-until terminal-parser display* output-string-parser
     parser)))

(define (parser:string:at-most n parser)
  (parser:get-output-string
   (parser:at-most n display* output-string-parser parser)))

(define (parser:string:at-most-until n terminal-parser parser)
  (parser:get-output-string
   (parser:at-most-until n terminal-parser display* output-string-parser
     parser)))

(define (parser:string:exactly n parser)
  (parser:get-output-string
   (parser:exactly n display* output-string-parser parser)))

(define (parser:string:at-least n parser)
  (parser:get-output-string
   (parser:at-least n display* output-string-parser parser)))

(define (parser:string:at-least-until n terminal-parser parser)
  (parser:get-output-string
   (parser:at-least-until n terminal-parser display* output-string-parser
     parser)))

(define (parser:string:between n m parser)
  (parser:get-output-string
   (parser:between n m display* output-string-parser parser)))

(define (parser:string:between-until n m terminal-parser parser)
  (parser:get-output-string
   (parser:between-until n m terminal-parser display* output-string-parser
     parser)))

(define (parser:bracketed-string left-bracket right-bracket parser)
  (parser:get-output-string
   (parser:bracketed* left-bracket right-bracket display* output-string-parser
     parser)))
