(in-package "LISP-UNIT-PROPOSAL")

;;;; TESTS
(defclass test ()
  ((name :initarg :name)
   (tags :initarg :tags)
   (package :initarg :package)
   (body :initarg :body)))

(defun test-name (test)
  (check-type test test)
  (slot-value test 'name))

(defun test-tags (test)
  (check-type test test)
  (slot-value test 'tags))

(defun test-package (test)
  (check-type test test)
  (slot-value test 'package))

(defun test-body (test)
  (check-type test test)
  (slot-value test 'body))

(defun execute-test (test)
  "Execute TEST and return all information about the execution in an
object of type TEST-RESULTS."
  (let ((fn (coerce (test-body test) 'function))
	(*results* (make-instance 'test-results)))
    (handler-case (funcall fn)
      (error (c)
	(record-test-signalled-error *results* c))
      (:no-error (&rest args)
	(declare (ignore args))
	(record-test-finished *results*)))
    *results*))

(defun testp (object)
  (typep object 'test))

(defmethod print-object ((object test) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (test-name object) :stream stream)
    (write-char #\Space stream)
    (let ((tags (test-tags object)))
      (if tags
	  (format stream "(~{~S~^ ~})" tags)
	  (format stream "()")))))

;;;; CONTAINERS
;;; - SIMPLE-TEST-CONTAINER
(defclass simple-test-container ()
  ((container
    :initarg :container))
  (:default-initargs
   :container (make-array 0 :adjustable t :fill-pointer 0)))

(defmethod print-object ((object simple-test-container) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((d (length (slot-value object 'container))))
      (case d
	(1
	 (format stream "1 test"))
	(t
	 (format stream "~d tests" d))))))

;;; - PACKAGE-TEST-CONTAINER
(defun package-test-container-p (object)
  (or (and (stringp object)
	   (find-package object))
      (packagep object)))

(deftype package-test-container ()
  `(satisfies package-test-container-p))

(defun canonical-package (package-designator)
  (etypecase package-designator
    (string
     (canonical-package (find-package package-designator)))
    (package
     package-designator)))

(defvar *package-containers* (make-hash-table :test 'equal)
  "A hash table where the keys are strings representing package
  names and the values are instances of TABLE-ITEM.")

(defclass table-item ()
  ((package
    :initarg :table-item-package
    :accessor table-item-package)
   (container
    :initarg :table-item-container
    :accessor table-item-container)))

(defun container-for-package (package)
  "Return the SIMPLE-TEST-CONTAINER for PACKAGE."
  (let* ((package (canonical-package package))
	 (package-name (package-name package))
	 (table-item (gethash package-name *package-containers*)))
    (cond
      ((and table-item (eql (table-item-package table-item)
			    package))
       (table-item-container table-item))
      (table-item
       (warn "Package object has changed for package name ~A." package-name)
       (setf (table-item-package table-item) package)
       (table-item-container table-item))      
      (t
       (let ((new-table-item (make-instance 'table-item
					    :table-item-package package
					    :table-item-container (make-instance 'simple-test-container))))
	 (setf (gethash (package-name package) *package-containers*) new-table-item)
	 (table-item-container new-table-item))))))

;;; - CONTAINER INTERFACE
(deftype test-container ()
  `(or simple-test-container
       package-test-container))

(defun ninsert-test (container test)
  (check-type test test)
  (etypecase container
    (simple-test-container
     (with-slots (container) container
       (let ((position (position (test-name test) container :key #'test-name)))
	 (if position
	     (setf (elt container position) test)
	     (vector-push-extend test container)))))
    (package-test-container
     (ninsert-test (print (container-for-package container)) test)))
  container)

(defun nremove-test (container test)
  (check-type test (or test (and symbol (not null))))
  (etypecase container
    (simple-test-container
     (with-slots (container) container
       (etypecase test
	 (test
	  (setf container (delete test container)))
	 ((and symbol (not null))
	  (setf container (delete test container :key #'test-name))))))
    (package-test-container
     (nremove-test (container-for-package container) test))))

(defun map-tests (function container)
  (etypecase container
    (simple-test-container
     (map nil function (slot-value container 'container)))
    (package-test-container
     (map-tests function (container-for-package container)))))

(defun number-of-tests (container)
  (etypecase container
    (simple-test-container
     (length (slot-value container 'container)))
    (package-test-container
     (number-of-tests (container-for-package container)))))

;;;; - TEST INTERFACE
(defun ensure-test (name package body &key tags)
  (let* ((object (make-instance 'test
			       :name name
			       :package package
			       :tags tags
			       :body body))
	 (container (container-for-package package)))
    (ninsert-test container object)))

(defun destructure-test-body (body)
  (assert (listp body))
  (destructuring-bind (&optional first-expression &rest everything-else)
      body    
    (cond
      ((and (listp first-expression)
	    (eql :tag (first first-expression)))
       (values everything-else (rest first-expression)))
      (t
       (values body nil)))))

(defmacro define-test (name &body body)
  (multiple-value-bind (test-body tags) (destructure-test-body body)
    (assert (every #'symbolp tags))
    `(ensure-test ',name *package* '(lambda () ,@test-body)
		  :tags (list ,@tags))))
