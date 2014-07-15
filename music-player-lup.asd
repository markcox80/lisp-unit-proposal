(defsystem "music-player-lup"
  :author "Mark Cox"
  :description "Demonstration system for the proposal. Use LISP-UNIT-PROPOSAL for tests."
  :depends-on ("lisp-unit-proposal")
  :components ((:module "mp/tests-lup"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "controller")
				     (:file "codec")))))
