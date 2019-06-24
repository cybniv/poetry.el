;;; poetry-update-test.el --- Tests for poetry.el

(ert-deftest poetry-update-should-update-dep ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-update)
    (poetry-wait-for-calls)
    (should (file-exists-p "poetry.lock"))))

;;; poetry-update-test.el ends here
