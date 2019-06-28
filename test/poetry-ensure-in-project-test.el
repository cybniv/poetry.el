;;; poetry-call-test.el --- Tests for poetry.el

(ert-deftest poetry-ensure-in-project-should-return-t-in-projects ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-ensure-in-project)))

(ert-deftest poetry-ensure-in-project-should-return-nil-outside-projects ()
  (poetry-test-cleanup)
  (should-error (poetry-ensure-in-project)))

;;; poetry-call-test.el ends here
