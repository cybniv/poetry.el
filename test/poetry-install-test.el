;;; poetry-install-test.el --- Tests for poetry.el


(ert-deftest poetry-install-should-install-dep ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-install)
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (and (re-search-forward
                    "^[[:space:]]*- Installing poetry.* (.*)$" nil t))))
    ;; should also create a lock
    (should (file-exists-p "poetry.lock"))))

;;; poetry-install-test.el ends here
