;;; poetry-check-test.el --- Tests for poetry.el

(ert-deftest poetry-check-should-check ()
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-check)
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "^All set!$" nil t)))))

;;; poetry-check-test.el ends here
