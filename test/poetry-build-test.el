;;; poetry-build-test.el --- Tests for poetry.el

(ert-deftest poetry-build-should-build ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-build)
    (poetry-wait-for-calls)
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "Building" nil t)))))

;;; poetry-build-test.el ends here
