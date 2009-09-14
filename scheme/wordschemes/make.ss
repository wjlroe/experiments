#lang scheme
(require compiler/bundle-dist)
(require compiler/distribute)
(require compiler/embed)

(create-embedding-executable
 "words"
 #:modules '((#t "words.ss"))
 #:mred? #t
 #:collects-path "/Applications/PLT Scheme v4.2/collects")

(assemble-distribution
 "WordSchemes"
 (list "words"))

;(bundle-directory
; "WordSchemes"
; "build"
; #t)