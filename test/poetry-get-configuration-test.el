;;; poetry-get-configuration-test.el --- Tests for poetry.el

(ert-deftest poetry-get-configuration-should-return-config ()
  (poetry-test-cleanup)
  ;; no error
  (poetry-get-configuration "virtualenvs.in-project")
  (should (string-match "virtualenv"
                        (poetry-get-configuration
                         "virtualenvs.path"))))

  (ert-deftest poetry-get-configuration-should-error-for-unrecognized-key ()
    (poetry-test-cleanup)
    (should-error (poetry-get-configuration "unrecognizable-key")))


;;; poetry-get-configuration-test.el ends here
