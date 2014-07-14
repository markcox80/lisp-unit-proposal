(in-package "LISP-UNIT-PROPOSAL")

(define-test-selection :tags (test-package-designator &rest args)
  `(:filter ,test-package-designator (:tags ,@args)))

(define-test-selection :package (test-package-designator)
  `(lambda ()
     (container-for-package ,test-package-designator)))

(define-test-selection :packages (&rest test-package-designators)
  `(lambda ()
     (apply #'concatenate-containers ,test-package-designators)))

(define-test-selection :filter (test-package-designator filter)
  `(lambda ()
     (filter-container ',filter ,test-package-designator)))

(define-test-selection :names (test-package-designator &rest names)
  `(lambda ()
     (let ((rv (make-test-container)))
       (map-tests #'(lambda (test)
		      (when (find (test-name test) ',names)
			(ninsert-test rv test)))
		  ,test-package-designator)
       rv)))
