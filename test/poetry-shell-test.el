;;; poetry-shell-test.el --- Tests for poetry.el

(ert-deftest poetry-shell-should-start-a-shell ()
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-shell)
    (goto-char (point-min))
    (re-search-forward "Spawning shell within" nil t)))

;;; poetry-shell-test.el ends here
