;;; poetry-run-test.el --- Tests for poetry.el


(ert-deftest poetry-run-should-run-command ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-run "ls")
    (with-current-buffer "*poetry*"
    (should (re-search-forward "^pyproject.toml$")))))

;;; poetry-run-test.el ends here
