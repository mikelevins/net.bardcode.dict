;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       net.bardcode.dict
;;;; Purpose:       package definition
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.dict
  (:use #:cl)
  (:export #:dict #:dict? #:immutable-dict? #:mutable-dict?
           #:all-keys #:all-values #:contains-key? #:contains-value?
           #:copy-dict #:empty? #:get-key #:merge-dicts
           #:put-key #:remove-key #:select-keys #:select-complement-keys
           #:mutable-dict #:merge-into! #:remove-key! #:set-key!))
