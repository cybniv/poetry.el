;;; poetry-lock-test.el --- Tests for poetry.el


(ert-deftest poetry-lock-should-lock-dep ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-lock)
    (poetry-wait-for-calls)
    (should (file-exists-p "poetry.lock"))))

;;; poetry-lock-test.el ends here
