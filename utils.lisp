;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       net.bardcode.dict
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2022 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.dict)

(defmethod take ((n (eql 0))(sequence cl:null)) (declare (ignore n sequence)) nil)
(defmethod take ((n integer)(sequence cl:null)) (error "Can't take ~s items from NIL" n))
(defmethod take ((n integer)(sequence cl:sequence))(cl:subseq sequence 0 (cl:min n (length sequence))))
