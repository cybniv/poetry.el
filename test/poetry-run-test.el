;;; poetry-run-test.el --- Tests for poetry.el


(ert-deftest poetry-run-should-run-command ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-run "ls")
    (with-current-buffer "*poetry-output*"
    (should (re-search-forward "^pyproject.toml$")))))

;; Test fail for an obscur travis related reason...
(unless (and (getenv "TRAVIS")
             (version< "26" emacs-version))
(ert-deftest poetry-run-should-run-interactively ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "ls")))
      (call-interactively 'poetry-run))
    (poetry-wait-for-calls)
    (with-current-buffer "*poetry-output*"
    (should (re-search-forward "^pyproject.toml$"))))))

;;; poetry-run-test.el ends here
