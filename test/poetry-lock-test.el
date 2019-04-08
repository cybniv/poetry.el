;;; poetry-lock-test.el --- Tests for poetry.el


(ert-deftest poetry-lock-should-lock-dep ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-lock)
    (should (file-exists-p "poetry.lock"))))

;;; poetry-lock-test.el ends here
