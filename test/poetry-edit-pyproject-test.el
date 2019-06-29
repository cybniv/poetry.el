;;; poetry-edit-pyproject-test.el --- Tests for poetry.el


(ert-deftest poetry-edit-pyproject-should-open-pyptoject-toml ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-edit-pyproject-toml)
    (should (string= (file-name-nondirectory (buffer-file-name)) "pyproject.toml"))))

;;; poetry-edit-pyproject-test.el ends here
