;;; poetry-self:update-test.el --- Tests for poetry.el


(ert-deftest poetry-self:update-should-self:update-dep ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    ;; Difficult to know what would happen, just check that it is not raising
    ;; any error
    (poetry-self-update)))

;;; poetry-self:update-test.el ends here
