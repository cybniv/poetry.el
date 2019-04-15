;;; poetry-publish-test.el --- Tests for poetry.el

(ert-deftest poetry-publish-should-publish ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-build)
    (poetry-publish "repo" "username" "pass")
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "^Publishing poetry.*$" nil t)))))

(ert-deftest poetry-publish-interactive-should-offer-completion ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-build)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "pypi"))
              ((symbol-function 'read-from-minibuffer) (lambda (&rest ignore) "username"))
              ((symbol-function 'read-passwd) (lambda (&rest ignore) "password")))
      (call-interactively 'poetry-publish))
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "^Publishing poetry.*$" nil t)))))

;;; poetry-publish-test.el ends here
