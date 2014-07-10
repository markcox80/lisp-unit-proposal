(in-package "LISP-UNIT-PROPOSAL")

(defclass test-filter-information ()
  ((name
    :initarg :name
    :reader test-filter-name)
   (lambda-list
    :initarg :lambda-list
    :reader test-filter-lambda-list)
   (body
    :initarg :body
    :reader test-filter-body)
   (function
    :initarg :function
    :reader test-filter-function)))

(defun ensure-test-filter (name function &key lambda-list body)
  (setf (get name 'test-filter) (make-instance 'test-filter-information
					       :name name
					       :lambda-list lambda-list
					       :body body
					       :function function)))

(defun test-filter-for-symbol (name &optional (error t))
  (let ((info (get name 'test-filter)))
    (check-type info (or null test-filter-information))
    (when (and (null info) error)
      (error "The test filter ~A is undefined." name))
    info))

(defmacro define-test-filter (name lambda-list &body body)
  `(ensure-test-filter ',name
		       #'(lambda ,lambda-list
			   ,@body)		       
		       :lambda-list ',lambda-list
		       :body ',body))

(defun expand-test-filter (expression)
  (cond
    ((and (listp expression) (eql 'lambda (first expression)))
     expression)
    ((symbolp expression)
     (let ((info (test-filter-for-symbol expression)))
       (expand-test-filter (funcall (test-filter-function info)))))
    ((listp expression)
     (destructuring-bind (name &rest args) expression
       (let ((info (test-filter-for-symbol name)))
	 (expand-test-filter (apply (test-filter-function info) args)))))
    (t
     (error "Invalid test filter expression: ~A." expression))))

(defun compile-test-filter (expression)
  (let ((lambda-expression (expand-test-filter expression)))
    (compile nil lambda-expression)))

(defun select-tests (filter container)
  (etypecase container
    (test-package
     (select-tests filter (container-for-package container)))
    (simple-test-container
     (let ((fn (if (functionp filter)
		   filter
		   (compile-test-filter filter)))
	   (rv (make-instance 'simple-test-container)))
       (map-tests #'(lambda (test)
		      (when (funcall fn test)
			(ninsert-test rv test)))
		  container)
       rv))))
