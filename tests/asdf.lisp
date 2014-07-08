(in-package "LISP-UNIT-PROPOSAL")

(defmethod asdf:perform ((op asdf:test-op) (cmp (eql (asdf:find-system "lisp-unit-proposal-tests"))))
  ;; Bootstrap tests
  (format t ";; Bootstrap tests.....")
  (handler-case (progn
		  (bootstrap-tests)
		  (format t "OK"))
    (error (c)
      (format t "FAILED")
      (terpri)
      (format t "~A" c)))
  (terpri))
