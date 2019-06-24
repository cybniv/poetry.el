;;; poetry-shell-test.el --- Tests for poetry.el

(ert-deftest poetry-shell-should-start-a-shell ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-opt-dep "atomicwrites")
    (poetry-add-opt-dep "attrs")
    (poetry-wait-for-calls)
    (poetry-shell)
    (goto-char (point-min))
    (re-search-forward "Spawning shell within" nil t)))

;;; poetry-shell-test.el ends here
