;;; poetry-init-test.el --- Tests for poetry.el


(ert-deftest poetry-init-should-run-init-command ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-empty-folder)))
    (find-file ppath)
    (poetry-init)
    (with-current-buffer "*poetry-init*"
      (sleep-for 1)
      (goto-char (point-min))
      (should (re-search-forward "This command will guide you through creating your")))))

(ert-deftest poetry-init-should-create-pyproject-toml ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-empty-folder)))
    (find-file ppath)
    (poetry-init)
    (with-current-buffer "*poetry-init*"
      (while (not (progn (goto-char (point-min))
                         (re-search-forward "finished" nil t)))
        (process-send-string (get-buffer-process (current-buffer))
                             "\n")
        (sleep-for .1))
      (should (file-exists-p
               (concat (file-name-as-directory (poetry-find-project-root))
                                               "pyproject.toml"))))))

(ert-deftest poetry-init-should-error-when-already-a-project ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (should-error (poetry-init))))

;;; poetry-init-test.el ends here
