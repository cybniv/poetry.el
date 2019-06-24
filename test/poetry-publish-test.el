;;; poetry-publish-test.el --- Tests for poetry.el

(ert-deftest poetry-publish-should-publish ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-build)
    (poetry-publish "repo" "username" "pass")
    (poetry-wait-for-calls)
    ;; Needed sometimes...
    (while (not (get-buffer "*poetry-error*"))
      (sleep-for .1))
    (should (with-current-buffer "*poetry-error*"
              (goto-char (point-min))
              (re-search-forward "Publishing" nil t)))))

(ert-deftest poetry-publish-interactive-should-offer-completion ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-build)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "pypi"))
              ((symbol-function 'read-string) (lambda (&rest ignore) "username"))
              ((symbol-function 'read-passwd) (lambda (&rest ignore) "password")))
      (call-interactively 'poetry-publish)
      (poetry-wait-for-calls)
      ;; Needed sometimes...
      (while (not (get-buffer "*poetry-error*"))
        (sleep-for .1))
      (should (with-current-buffer "*poetry-error*"
                (goto-char (point-min))
                (re-search-forward "Publishing" nil t))))))

;;; poetry-publish-test.el ends here
