(defsystem "music-player"
  :author "Mark Cox"
  :description "Demonstration system for the proposal. Use current version of LISP-UNIT for tests."
  :depends-on ("lisp-unit")
  :components ((:module "mp/tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "controller")
				     (:file "codec")))))
