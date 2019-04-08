;;; poetry-build-test.el --- Tests for poetry.el

(ert-deftest poetry-build-should-build ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-build)
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "^Building poetry.*$" nil t)))))

;;; poetry-build-test.el ends here
