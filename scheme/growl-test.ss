#lang scheme

(require (planet murphy/growl:1:2/growl))

(define notifications
  '([#:id "normal" #:type "NormalScheme"]))

(define growl-serv 
  (new growl% [name "Scheme Growl"] [notifications notifications]))

(send growl-serv notify :normal #:title "Growl notification from Scheme" #:description "woot")

(send growl-serv close)
