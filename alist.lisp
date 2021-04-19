;;;; ***********************************************************************
;;;;
;;;; Name:          alist.lisp
;;;; Project:       net.bardcode.dict
;;;; Purpose:       alist type and utilities
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.dict)


;;; an alist is a list all of whose elements are lists

(defmethod alist? (thing)
  (declare (ignore thing))
  t)

(defmethod alist? ((thing cons)) 
  (and thing
       (every 'consp thing)))

(deftype alist ()
  `(and list (satisfies alist?)))


(defmethod alist-to-plist ((alist null))
  (declare (ignore alist))
  nil)

(defmethod alist-to-plist ((alist list))
  (loop for pair in alist
     append (list (car pair)(cdr pair))))
