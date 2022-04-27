;;;; ***********************************************************************
;;;;
;;;; Name:          mutable-dict.lisp
;;;; Project:       net.bardcode.dict
;;;; Purpose:       mutable finite maps using alists for entries
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.dict)

;;;------------------------------------------------------------------------------------------
;;; class mutable-dict
;;;------------------------------------------------------------------------------------------
;;; a mutable dict class that stores entries in an alist

(defclass mutable-dict (dict)())

;;; mutable dicts

(defun mutable-dict (key-test &rest contents)
  (let ((entries (loop for tail on contents by #'cddr
                    collect (cons (first tail)(second tail)))))
    (make-instance 'mutable-dict :key-test key-test :entries entries)))

(defmethod mutable-dict? (thing) nil)
(defmethod mutable-dict? ((dict mutable-dict)) t)


;;; mutable-dict protocol
;;;------------------------------------------------------------------------------------------

(defmethod merge-into! ((left mutable-dict) (right dict))
  (let ((key-test (key-test left)))
    (loop for entry in (entries right)
       do (let ((found-entry (assoc (car entry) (entries left) :test key-test)))
            (if found-entry
                (setf (cdr found-entry) (cdr entry))
                (setf (entries left)
                      (cons (cons (car entry)(cdr entry))
                            (entries left))))))
    left))

(defmethod remove-key! ((dict mutable-dict) key &key test &allow-other-keys)
  (let ((found-entry (assoc key (entries dict) :test (key-test dict))))
    (when found-entry
      (let ((new-entries (remove key (entries dict) :test (key-test dict) :key 'car)))
        (setf (entries dict) new-entries)))
    dict))

(defmethod set-key! ((dict mutable-dict) key value &key test &allow-other-keys)
  (let ((already-entry (assoc key (entries dict) :test (or test (key-test dict)))))
    (if already-entry
        (setf (cdr already-entry) value)
        (setf (entries dict)
              (cons (cons key value)
                    (entries dict))))
    dict))

;;; (setf $dict1 (dict 'equal "name" "Fred" "age" 35))
;;; (all-keys $dict1)
;;; (all-values $dict1)
;;; (contains-key? $dict1 "name")
;;; (contains-value? $dict1 "Fred")
;;; (setf $dict2 (copy-dict $dict1))
;;; (get-key $dict2 "name")
;;; (get-key $dict2 "shape" :default :none)
;;; (setf $dict3 (merge-keys $dict1 (dict 'equal "name" "Barney")))
;;; (setf $dict4 (put-key $dict1 "color" "orange"))
;;; (remove-key $dict4 "age")
;;; (select-keys $dict4 (list "age" "color" "shape"))
;;; (setf $dict5 (mutable-dict 'equal "name" "Fred" "age" 35))
;;; (all-keys $dict5)
;;; (merge-into! $dict5 (dict 'equal "name" "Barney" "size" "small"))
;;; (set-key! $dict5 "size" "little")
;;; (set-key! $dict5 "color" "brown")
;;; (remove-key! $dict5 "size")
