;;;; dict.asd

(asdf:defsystem #:net.bardcode.dict
  :description "A simple finite map class using alists for entries"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (:net.bardcode.alist)
  :components ((:file "package")
               (:file "alist")
               (:file "plist")
               (:file "dict")
               (:file "mutable-dict")))

;;; (asdf:load-system :net.bardcode.dict)
;;; (ql:quickload :net.bardcode.dict)
