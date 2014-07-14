(in-package "LISP-UNIT-PROPOSAL")

(defun ninsert-container (destination source)
  (map-tests #'(lambda (test)
		 (ninsert-test destination test))
	     source)
  destination)

(defun concatenate-containers (&rest containers)
  (let ((rv (make-test-container)))
    (dolist (container containers)
      (ninsert-container rv container))
    rv))

(defun test-selection-from-name (symbol)
  (get symbol 'test-selection))

(defun test-selection-name-p (symbol)
  (not (null (test-selection-from-name symbol))))

(defclass test-selection-data ()
  ((name
    :initarg :name
    :reader test-selection-name)
   (function
    :initarg :function
    :reader test-selection-function)))

(defun ensure-test-selection (name function)
  (setf (get name 'test-selection) (make-instance 'test-selection-data
						  :name name
						  :function function))
  name)

(defmacro define-test-selection (name test-selection-lambda-list &body body)
  `(ensure-test-selection ',name #'(lambda ,test-selection-lambda-list
				     ,@body)))

(defun select-tests (&rest test-selections)
  (let ((rv (make-test-container)))
    (dolist (test-selection test-selections)
      (let ((fn (compile-test-selection test-selection)))
	(ninsert-container rv (funcall fn))))
    rv))

(defun compile-test-selection (test-selection)
  (compile nil (expand-test-selection test-selection)))

(defun expand-test-selection (test-selection)
  (let ((test-selection (expand-test-selection-1 test-selection)))
    (cond
      ((and (listp test-selection) (eql 'lambda (first test-selection)))
       test-selection)
      (t
       (expand-test-selection test-selection)))))

(defun expand-test-selection-1 (test-selection)
  (cond
    ((and (listp test-selection) (eql 'lambda (first test-selection)))
     test-selection)
    ((and (symbolp test-selection) (not (null test-selection)))
     (let ((data (test-selection-from-name test-selection)))
       (funcall (test-selection-function data))))
    ((listp test-selection)
     (destructuring-bind (name &rest args) test-selection
       (let ((data (test-selection-from-name name)))
	 (apply (test-selection-function data) args))))
    (t
     (error "Invalid test selection expression ~A" test-selection))))

(defvar *print-summary* t)
(defun run-test-selection (&rest forms)
  (let* ((container (apply #'select-tests forms))
	 (results (run-tests-in-container container)))
    (when *print-summary*
      (print-summary results))
    results))

(defun run-tests (test-names package)
  (cond
    ((eql test-names :all)
     (run-test-selection `(:package ,package)))
    ((listp test-names)
     (run-test-selection `(:names ,package ,@test-names)))))
