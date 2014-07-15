(defpackage "LISP-UNIT-PROPOSAL"
  (:use "COMMON-LISP")
  
  ;; Tests
  (:export #:define-test)

  ;; Assertions
  (:export #:assert-true)

  ;; Results
  (:export #:print-summary
	   #:print-errors)

  ;; Test Filters
  (:export #:define-test-filter
	   #:compile-test-filter
	   #:expand-test-filter
	   #:expand-test-filter-1)

  ;; Test Selections
  (:export #:define-test-selection
	   #:compile-test-selection
	   #:expand-test-selection
	   #:expand-test-selection-1)

  ;; Execution
  (:export #:run-tests
	   #:run-test-selection))
