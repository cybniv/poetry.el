;;; poetry-get-configuration-test.el --- Tests for poetry.el

(ert-deftest poetry-get-configuration-should-return-config ()
  (poetry-test-cleanup)
  ;; no error
  (poetry-get-configuration "settings.virtualenvs.in-project")
  (should (string-match "virtualenv"
                        (poetry-get-configuration
                         "settings.virtualenvs.path"))))

  (ert-deftest poetry-get-configuration-should-error-for-unrecognized-key ()
    (poetry-test-cleanup)
    (should-error (poetry-get-configuration "unrecognizable-key")))


;;; poetry-get-configuration-test.el ends here
