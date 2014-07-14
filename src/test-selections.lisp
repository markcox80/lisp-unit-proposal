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

(defun ensure-test-selection (name &rest forms)
  (let* ((functions (mapcar #'(lambda (form)
				(coerce (lambda-form-for-test-selection-form form)
					'function))
			    forms))
	 (fn #'(lambda ()
		 (apply #'concatenate-containers (mapcar #'funcall functions)))))
    (setf (get name 'test-selection) (make-instance 'test-selection-data
						    :name name
						    :function fn))
    name))

(defun lambda-form-for-test-selection-form (form)
  (cond
    ((and (symbolp form) form)
     `(lambda ()
	(select-tests ',form)))
    ((listp form)
     (alexandria:destructuring-ecase form
       ((:tags test-package-designator &rest args)
	`(lambda ()
	   (filter-container (list :tags ,@args) ,test-package-designator)))
       ((:package test-package-designator)
	`(lambda ()
	   (container-for-package ,test-package-designator)))
       ((:packages &rest packages)
	`(lambda ()
	   (concatenate-containers ,@packages)))
       ((:filter test-package-designator filter)
	`(lambda ()
	   (filter-container ',filter ,test-package-designator)))
       ((:names test-package-designator &rest names)
	`(lambda ()
	   (let ((rv (make-test-container)))
	     (map-tests #'(lambda (test)
			    (when (find (test-name test) ',names)
			      (ninsert-test rv test)))
			,test-package-designator)
	     rv)))))
    (t
     (error "Invalid form: ~A" form))))

(defmacro define-test-selection (name &body body)
  `(ensure-test-selection ',name ,@(mapcar #'(lambda (form)
					       `',form)
					   body)))

(defun select-tests (&rest test-selections)
  (let ((rv (make-test-container)))
    (dolist (test-selection test-selections)
      (cond
	((and (symbolp test-selection) test-selection)
	 (let ((data (test-selection-from-name test-selection)))
	   (ninsert-container rv (funcall (test-selection-function data)))))
	((listp test-selection)
	 (ninsert-container rv (funcall (coerce (lambda-form-for-test-selection-form test-selection)
						'function))))
	(t
	 (error "Invalid test selection form: ~A" test-selection))))
    rv))

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
