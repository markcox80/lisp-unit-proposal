(in-package "LISP-UNIT-PROPOSAL")

(define-test bootstrap-test/success
  (:tag :bootstrap)
  (assert-true t))

(define-test bootstrap-test/failure
  (:tag :bootstrap)
  (assert-true nil)
  (assert-true nil))

(define-test bootstrap-test/unhandled-condition
  (:tag :bootstrap)
  (error "This error should be handled by EXECUTE-TEST."))

(defun bootstrap-tests ()
  (labels ((find-test (test-name container)
	     (etypecase container
	       (simple-test-container
		(find test-name (slot-value container 'container) :key #'test-name))
	       (package-test-container
		(find-test test-name (container-for-package container))))))

    (let ((test (find-test 'bootstrap-test/success "LISP-UNIT-PROPOSAL")))
      (assert test)
      (assert (equal '(:bootstrap) (test-tags test)))
      (let ((results (execute-test test)))
	(assert (= 1 (assertion-count results)))
	(assert (= 1 (successful-assertions-count results)))
	(assert (= 0 (failed-assertions-count results)))
	(assert (null (unhandled-condition results)))))

    (let ((test (find-test 'bootstrap-test/failure "LISP-UNIT-PROPOSAL")))
      (assert test)
      (assert (equal '(:bootstrap) (test-tags test)))
      (let ((results (execute-test test)))
	(assert (= 2 (assertion-count results)))
	(assert (= 0 (successful-assertions-count results)))
	(assert (= 2 (failed-assertions-count results)))
	(assert (null (unhandled-condition results)))))

    (let ((test (find-test 'bootstrap-test/unhandled-condition "LISP-UNIT-PROPOSAL")))
      (assert test)
      (assert (equal '(:bootstrap) (test-tags test)))
      (let ((results (execute-test test)))
	(assert (= 0 (assertion-count results)))
	(assert (= 0 (successful-assertions-count results)))
	(assert (= 0 (failed-assertions-count results)))
	(assert (unhandled-condition results))))))

