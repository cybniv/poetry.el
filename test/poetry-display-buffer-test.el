;;; poetry-call-test.el --- Tests for poetry.el

(ert-deftest poetry-display-buffer-should-display-current-buffer ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-wait-for-calls)
    (poetry-display-buffer)
    (other-window 1)
    (should (string= (buffer-name) "*poetry*"))))

(ert-deftest poetry-display-buffer-should-display-error-buffer ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "atomicwrites")
    (poetry-wait-for-calls)
    (poetry-display-buffer "*poetry-error*")
    (other-window 1)
    (should (string= (buffer-name) "*poetry-error*"))))



;;; poetry-call-test.el ends here
