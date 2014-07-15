(in-package "MUSIC-PLAYER.TESTS-LUP")

(defmethod asdf:perform ((op asdf:test-op) (cmp (eql (asdf:find-system "music-player-lup"))))
  (run-test-selection '(:packages "MUSIC-PLAYER.CONTROLLER.TESTS-LUP"
			          "MUSIC-PLAYER.CODEC.TESTS-LUP")))
