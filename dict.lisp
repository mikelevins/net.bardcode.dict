;;;; ***********************************************************************
;;;;
;;;; Name:          dict.lisp
;;;; Project:       net.bardcode.dict
;;;; Purpose:       finite maps using alists for entries
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.dict)

;;;------------------------------------------------------------------------------------------
;;; class dict
;;;------------------------------------------------------------------------------------------
;;; an immutable dict class that stores entries in an alist

(defclass dict ()
  ((key-test :accessor key-test :initform 'equal :initarg :key-test)
   (entries :accessor entries :initform nil :initarg :entries)))

(defmethod print-object ((dict dict) stream)
  (let* ((entries (entries dict))
         (first-entry (first entries))
         (rest-entries (rest entries)))
    (format stream "{")
    (when first-entry
      (format stream "~S ~S" (car first-entry)(cdr first-entry)))
    (loop for entry in rest-entries
       do (let ((*print-pretty* nil))
            (format stream " ~S ~S" (car entry)(cdr entry))))
    (format stream "}")))

(defun dict (key-test &rest contents)
  (let ((entries (loop for tail on contents by #'cddr
                    collect (cons (first tail)(second tail)))))
    (make-instance 'dict :key-test key-test :entries entries)))

(defmethod dict? (thing) nil)
(defmethod dict? ((thing dict)) t)

(defmethod immutable-dict? (thing) nil)
(defmethod immutable-dict? ((thing dict)) t) 

;;; dict protocol
;;;------------------------------------------------------------------------------------------

(defmethod all-keys ((dict dict) &key &allow-other-keys)
  (mapcar 'car (entries dict)))

(defmethod all-values ((dict dict) &key &allow-other-keys)
  (mapcar 'cdr (entries dict)))

(defmethod all-entries ((dict dict) &key &allow-other-keys)
  (entries dict))

(defmethod contains-key? ((dict dict) key)
  (if (assoc key (entries dict) :test (key-test dict))
      t
      nil))

(defmethod contains-value? ((dict dict) value &key (test 'equal))
  (and (find-if (lambda (e)(funcall test value (cdr e)))
                (entries dict))
       t))

(defmethod copy-dict ((dict dict))
  (make-instance (class-of dict)
                 :key-test (key-test dict)
                 :entries (copy-tree (entries dict))))

(defmethod empty-dict? ((dict dict) &key &allow-other-keys)
  (null (entries dict)))

(defmethod get-key ((dict dict) key &key (default nil))
  (let ((entry (assoc key (entries dict) :test (key-test dict))))
    (if entry
        (cdr entry)
        default)))

;;; creates a new dict from left with the keys from right added.
;;; the new dict uses the key-test from left
;;; if we find a duplicate key in right, we use its value, replacing
;;; the value in left.

(defmethod binary-merge-keys ((left dict) (right dict) &key (result-class nil))
  (let* ((result-class (or result-class (class-of left)))
         (key-test (key-test left))
         (left-entries (entries left))
         (right-entries (entries right))
         (new-entries (copy-tree left-entries)))
    (loop for e in right-entries
       do (let ((already-entry (assoc (car e) new-entries :test key-test)))
            (if already-entry
                (setf (cdr already-entry)
                      (cdr e))
                (setf new-entries
                      (cons (cons (car e)
                                  (cdr e))
                            new-entries)))))
    (make-instance result-class :key-test key-test :entries new-entries)))

;;; (setf $dict1 (dict 'equal "name" "Fred" "age" 35))
;;; (setf $dict3 (binary-merge-keys $dict1 (dict 'equal "name" "Barney")))
;;; (class-of $dict3)

(defmethod merge-keys ((left dict) &rest dicts)
  (reduce 'binary-merge-keys dicts :initial-value left))

;;; (setf $dict1 (dict 'equal "name" "Barney" "age" 35))
;;; (setf $dict2 (dict 'equal "name" "Fred" "color" :orange))
;;; (setf $dict3 (dict 'equal "shape" :square))
;;; (setf $dict4 (merge-keys $dict1 $dict2 $dict3))

(defmethod put-key ((dict dict) key value &key (test 'equal) (default nil))
  (let* ((already-entry (assoc key (entries dict) :test (key-test dict)))
         (new-entry (cons key value))
         (new-entries (if already-entry
                          (remove key (entries dict) :test (key-test dict) :key 'car)
                          (entries dict))))
    (make-instance (class-of dict)
          :key-test (key-test dict)
          :entries (cons new-entry new-entries))))

(defmethod remove-key ((dict dict) key &key test &allow-other-keys)
  (let* ((found-entry (assoc key (entries dict) :test (key-test dict)))
         (new-entries (if found-entry
                          (remove key (entries dict) :test (key-test dict) :key 'car)
                          (copy-tree (entries dict)))))
    (make-instance (class-of dict)
          :key-test (key-test dict)
          :entries new-entries)))

(defmethod select-keys ((dict dict) keys &key test &allow-other-keys)
  (let ((key-test (key-test dict)))
    (make-instance (class-of dict)
          :key-test key-test
          :entries (remove-if-not (lambda (entry)(member (car entry) keys :test (or test (key-test dict))))
                                  (copy-tree (entries dict))))))

(defmethod select-complement-keys ((dict dict) keys &key test &allow-other-keys)
  (let ((key-test (key-test dict)))
    (make-instance (class-of dict)
                   :key-test key-test
                   :entries (remove-if (lambda (entry)(member (car entry) keys :test (or test (key-test dict))))
                                       (copy-tree (entries dict))))))


;;; select-keys-if dict test
;;; ---------------------------------------------------------------------
;;; select keys of the dict for which the function test reutrns true
;;; test is a function of the form (lambda (key value)...) => Boolean

(defmethod select-keys-if ((dict dict)(test function))
  (let ((passing-keys (remove-if-not (lambda (k)(funcall test k (get-key dict k :default nil)))
                                 (all-keys dict))))
    (select-keys dict passing-keys)))

;;; (setf $d (dict 'equal :a 1 :b 2 :c 3 :d 4))
;;; (all-keys $d)
;;; (select-keys $d '(:a :c))
;;; (select-complement-keys $d '(:a :c))
;;; (select-keys-if $d (lambda (k v)(zerop v)))
