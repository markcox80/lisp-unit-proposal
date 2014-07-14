(defsystem "lisp-unit-proposal"
  :author "Mark Cox"
  :description "A working implementation of a proposal for the LISP-UNIT system."
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "results")
				     (:file "tests")
				     (:file "test-filters")
				     (:file "builtin-test-filters")
				     (:file "test-selections")
				     (:file "builtin-test-selections"))))
  :in-order-to ((test-op (test-op "lisp-unit-proposal-tests"))))
