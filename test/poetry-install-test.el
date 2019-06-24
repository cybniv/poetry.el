;;; poetry-install-test.el --- Tests for poetry.el


(ert-deftest poetry-install-should-install-dep ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-install)
    (poetry-wait-for-calls)
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "- Installing" nil t)))
    ;; should also create a lock
    (should (file-exists-p "poetry.lock"))))

;;; poetry-install-test.el ends here
