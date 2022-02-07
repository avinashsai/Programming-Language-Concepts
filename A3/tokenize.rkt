#lang racket
;; To use this module, save this code as tokenize.rkt in the same directory as your
;; code and add the following line to your file:
;;
;; (require (only-in (file "tokenize.rkt") tokenize))

(provide tokenize)

;; The `[data #f]` is a default value for the 2nd positional argument.
;; That way, this function can take 1 arg or 2 args.
(define (token type [data #f])
  (list type data))

;;;; Token creator functions
;;;;
;;;; Given a string matching a regexp, return a token for that input or #f for
;;;; no token.

(define (skip-match str) #f)

(define (punctuation-token str)
  (token
    (case str
      [("(") 'OPAREN]
      [(")") 'CPAREN])))

(define (number-token type)
  ;; Note the indirection here: the first call returns a function of 1 argument.
  (λ (str) (token type (string->number str))))

(define (string-token str)
  (let* ([len (string-length str)]
         [inner-string (substring str 1 (sub1 len))] ; strip first & last quotes
         [unescaped-char (λ (_ char) (if (equal? "n" char) "\n" char))]
         [unescaped-str (regexp-replace #rx"\\\\(.)"
                                        inner-string unescaped-char)])
    (token 'STRING unescaped-str)))

(define (name-token str)
  (token 'NAME (string->symbol str)))

;;;; Tokenizing rules table
;;;;
;;;; Each item in the table is a 2-tuple (i.e. list of 2 elements):
;;;; 1. a regexp to detect a token at the beginning of a string
;;;; 2. a function (from above) to take the matched string and create a token

(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; whitespace
    (list #rx"^;\\*.*?\\*;" skip-match) ; /* */ comments
    (list #rx"^;;[^\n]+(\n|$)" skip-match) ; // comments
    (list #rx"^[()]" punctuation-token)
    (list #rx"^-?[0-9]+\\.[0-9]+(?=[\r\n\t (){},;.]|$)" (number-token 'FLOAT))
    (list #rx"^-?[0-9]+(?=[\r\n\t (){},;.]|$)" (number-token 'INT))
    (list #rx"^\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\"(?=[\r\n\t (){},;.]|$)"
          string-token)
    (list #rx"^[^(){},;.\" \r\n\t0-9][^(){},;.\" \r\n\t]*(?=[\r\n\t (){},;.]|$)"
          name-token)))

;;;; Tokenizing engine

;; Given the current input string and a tokenizing rule tuple, try to match. If no
;; match, return #f. If match, return the match and the corresponding token.
(define (table-entry->result input entry)
  (let* ([regexp (first entry)]
         [process-match (second entry)]
         [matches (regexp-match regexp input)])
    (if (not matches)
      #f
      (let* ([match (first matches)]
             [token (process-match match)])
        (list (string-length match) match token)))))

;; Break the input string up into a list of tokens.
;; This function is recursive, returning a pair of the front token and the
;; result of a recursive call on the remaining input.
(define (tokenize input)
  (if (zero? (string-length input))
    null
    ;; filter-map calls map (which calls the function once per item) and
    ;; removes #f results.
    (let ([match-results (filter-map (λ (entry) (table-entry->result input entry))
                                     re-table)])
      (if (empty? match-results)
        (list (token 'INVALID input))
        (let* ([longest-match-result (first (sort match-results > #:key first))]
               [longest-match-length (first longest-match-result)]
               [longest-match (second longest-match-result)]
               [token (third longest-match-result)]
               [remaining-input (substring input longest-match-length)])
          (if token
            (cons token (tokenize remaining-input))
            (tokenize remaining-input)))))))
