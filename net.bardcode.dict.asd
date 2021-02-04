;;;; dict.asd

(asdf:defsystem #:net.bardcode.dict
  :description "A simple finite map class using alists for entries"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "dict")))

;;; (asdf:load-system :net.bardcode.dict)
;;; (ql:quickload :net.bardcode.dict)
