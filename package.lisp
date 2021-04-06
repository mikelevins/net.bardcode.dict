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
  (:export #:dict #:dict? #:immutable-dict? #:mutable-dict #:mutable-dict?
           #:all-entries #:all-keys #:all-values #:contains-key? #:contains-value?
           #:copy-dict #:empty? #:entries #:get-key #:key-test #:merge-keys
           #:put-key #:remove-key #:select-keys #:select-complement-keys #:select-keys-if
           #:mutable-dict #:merge-into! #:remove-key! #:set-key!))
