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
  (:use #:cl #:net.bardcode.alist)
  (:nicknames :dict)
  (:export
   #:*maximum-printed-dict-entries*
   #:alist #:alist? #:alist-to-plist #:all-entries #:all-keys #:all-values
   #:binary-merge-keys
   #:contains-key? #:contains-value? #:copy-dict
   #:dict #:dict? #:dict-to-alist #:dict-to-plist
   #:empty? #:entries
   #:get-key
   #:immutable-dict?
   #:key-test
   #:merge-keys #:merge-into! #:mutable-dict #:mutable-dict?
   #:plist #:plist? #:plist-to-alist #:put-key
   #:remove-key #:remove-key!
   #:select-complement-keys #:select-keys #:select-keys-if #:set-key!
   ))
