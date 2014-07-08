(in-package "MUSIC-PLAYER.TESTS")

#+:lisp-unit
(defmethod asdf:perform ((op asdf:test-op) (cmp (eql (asdf:find-system "music-player"))))
  (dolist (pkg (list "MUSIC-PLAYER.CONTROLLER.TESTS"
		     "MUSIC-PLAYER.CODEC.TESTS"))
    (format t ";; Running tests in package: ~A~%" pkg)
    (lisp-unit:run-tests :all pkg)))

#+:lisp-unit-proposal
(defmethod asdf:perform ((op asdf:test-op) (cmp (eql (asdf:find-system "music-player-proposal"))))
  (run-test-collections "MUSIC-PLAYER.CONTROLLER.TESTS"
			"MUSIC-PLAYER.CODEC.TESTS"))
