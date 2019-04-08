;;; poetry-new-project-test.el --- Tests for poetry.el


(ert-deftest poetry-new-should-create-a-new-project ()
  (let ((ppath (poetry-test-create-empty-folder)))
    (poetry-new ppath)
  (let ((pyproj-path (concat (file-name-as-directory ppath)
                                  "pyproject.toml"))
        (readme-path (concat (file-name-as-directory ppath)
                                  "README.rst"))
        (tests-path (concat (file-name-as-directory ppath)
                                  "tests")))
    (should (file-exists-p pyproj-path))
    (should (file-exists-p readme-path))
    (should (file-exists-p tests-path)))))

;;; poetry-new-project-test.el ends here
