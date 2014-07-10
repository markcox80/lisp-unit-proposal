(in-package "LISP-UNIT-PROPOSAL")

;;;; TEST RESULTS
(defclass test-results ()
  ((assertion-count
    :initarg :assertion-count
    :accessor assertion-count)
   (successful-assertions-count
    :initarg :successful-assertions-count
    :accessor successful-assertions-count)
   (failed-assertions
    :initarg :failed-assertions
    :accessor failed-assertions)
   (unhandled-condition
    :initarg :unhandled-condition
    :accessor unhandled-condition))
  (:documentation "Stores details about results for a single test.")
  (:default-initargs
   :assertion-count 0
   :successful-assertions-count 0
   :failed-assertions nil
   :unhandled-condition nil))

(defun failed-assertions-count (test-results)
  (- (assertion-count test-results)
     (successful-assertions-count test-results)))

(defun record-assertion-failed (test-results assertion)
  (incf (assertion-count test-results))
  (push assertion (failed-assertions test-results)))

(defun record-assertion-succeeded (test-results assertion)
  (declare (ignore assertion))
  (incf (assertion-count test-results))
  (incf (successful-assertions-count test-results)))

(defun record-test-signalled-error (test-results condition)
  (setf (unhandled-condition test-results) condition)
  (alexandria:nreversef (failed-assertions test-results)))

(defun record-test-finished (test-results)
  (alexandria:nreversef (failed-assertions test-results)))

;;;; ASSERTIONS
(defvar *results* nil
  "A TEST-RESULTS object used to store the results of a test.")

(defmacro capture-extras (&rest extras)
  "Return a form which evaluates to an ALIST containing the objects
bound to each symbol in EXTRAS."
  `(list ,@(mapcar #'(lambda (extra)
		       `(cons ',extra ,extra))
		   extras)))

(defclass assertion/assert-true ()
  ((form
    :initarg :form
    :reader form)
   (extras
    :initarg :extras
    :reader extras)))

(defmacro assert-true (form &rest extras)
  `(if ,form
       (record-assertion-succeeded *results* 'assert-true)
       (record-assertion-failed *results* (make-instance 'assertion/assert-true
							 :form ',form
							 :extras (capture-extras ,@extras)))))

;;;; TEST RESULTS COLLECTION
(defclass test-results-collection ()
  ((assertion-count
    :initarg :assertion-count
    :accessor assertion-count)
   (successful-assertions-count
    :initarg :successful-assertions-count
    :accessor successful-assertions-count)
   (unhandled-condition-count
    :initarg :unhandled-condition-count
    :accessor unhandled-condition-count)
   (missing-count
    :initarg :missing-count
    :accessor missing-count)
   (test-results-table
    :initarg :test-results-table
    :reader test-results-table))
  (:documentation "Stores details about results for a collection of tests.")
  (:default-initargs
   :assertion-count 0
   :successful-assertions-count 0
   :unhandled-condition-count 0
   :missing-count 0
   :test-results-table (make-hash-table :test 'eql)))

(defun ninsert-test-results (results-collection test results)
  (let ((test-results-table (test-results-table results-collection)))
    (assert (not (gethash (test-name test) test-results-table)))
    (incf (assertion-count results-collection) (assertion-count results))
    (incf (successful-assertions-count results-collection) (successful-assertions-count results))
    (when (unhandled-condition results)
      (incf (unhandled-condition-count results-collection)))
    (setf (gethash (test-name test) test-results-table) results)))

(defun print-summary (results &optional (stream *standard-output*))
  (etypecase results
    (test-results-collection
     (format stream "Unit Test Summary~%")
     (format stream " | ~d assertions total.~%" (assertion-count results))
     (format stream " | ~d passed.~%" (successful-assertions-count results))
     (format stream " | ~d failed.~%" (failed-assertions-count results))
     (format stream " | ~d execution errors.~%" (unhandled-condition-count results))
     (format stream " | ~d missing tests.~%" (missing-count results)))))

(defun print-errors (results &optional (stream *standard-output*))
  (etypecase results
    (test-results-collection
     (maphash #'(lambda (key value)
		  (declare (ignore key))
		  (print-errors value stream))
	      (test-results-table results)))
    (test-results
     (let ((c (unhandled-condition results)))
       (when c
	 (write c :stream stream))))))
