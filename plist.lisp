;;;; ***********************************************************************
;;;;
;;;; Name:          plist.lisp
;;;; Project:       net.bardcode.dict
;;;; Purpose:       plist type and utilities
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.dict)


;;; a plist is a list with an even number of elements whose
;;; even-indexed elements are atoms

(defmethod plist? (thing)
    (declare (ignore thing))
    t)

(defmethod plist? ((thing cons)) 
  (and thing
       (block testing
         (loop for tail on thing by #'cddr
            do (unless (and (cdr tail)
                            (atom (car tail)))
                 (return-from testing nil)))
         t)))

(deftype plist ()
  `(and list (satisfies plist?)))


(defmethod plist-to-alist ((plist null))
  (declare (ignore plist))
  nil)

(defmethod plist-to-alist ((plist list))
  (loop for tail on plist by #'cddr
     collect (cons (car tail)
                   (cadr tail))))
