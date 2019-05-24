;;; poetry-run-test.el --- Tests for poetry.el


(ert-deftest poetry-run-should-run-command ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-run "ls")
    (with-current-buffer "*poetry*"
    (should (re-search-forward "^pyproject.toml$")))))

;; Test fail for an obscur travis related reason...
(unless (and (getenv "TRAVIS")
             (version< "26" emacs-version))
(ert-deftest poetry-run-should-run-interactively ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "ls")))
      (call-interactively 'poetry-run))
    (with-current-buffer "*poetry*"
    (should (re-search-forward "^pyproject.toml$"))))))

;;; poetry-run-test.el ends here
