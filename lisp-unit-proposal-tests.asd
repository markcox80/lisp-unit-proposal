(defsystem "lisp-unit-proposal-tests"
  :author "Mark Cox"
  :description "Tests for the LISP-UNIT-PROPOSAL system."
  :serial t
  :depends-on ("lisp-unit-proposal")
  :components ((:module "tests"
			:serial t
			:components ((:file "bootstrap-tests")
				     (:file "asdf")))))
