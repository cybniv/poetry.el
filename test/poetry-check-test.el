;;; poetry-check-test.el --- Tests for poetry.el

(ert-deftest poetry-check-should-check ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-check)
    (poetry-wait-for-calls)
    (should (with-current-buffer "*poetry-output*"
              (goto-char (point-min))
              (re-search-forward "All set!" nil t)))))

;;; poetry-check-test.el ends here
