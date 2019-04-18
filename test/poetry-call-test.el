;;; poetry-call-test.el --- Tests for poetry.el

(ert-deftest poetry-call-should-error-when-no-executable ()
  (let ((ppath (poetry-test-create-empty-folder))
        (exec-path '()))
    (should-error (poetry-new ppath))))

;;; poetry-call-test.el ends here
