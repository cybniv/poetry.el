;;; poetry-show-test.el --- Tests for poetry.el

(ert-deftest poetry-show-should-display-packages ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-wait-for-calls)
    (should (string-match
             (concat
              "six.*"
              "pytest.*"
              "py.*"
              "pluggy.*"
              "more-itertools.*"
              "attrs.*"
              "atomicwrites.*")
             (substring-no-properties
              (apply 'concat (poetry-show-get-packages)))))))

(ert-deftest poetry-show-should-display-package-info ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "six")
    (poetry-show "six")
    (poetry-wait-for-calls)
    (with-current-buffer "*poetry-output*"
      (goto-char (point-min))
      (should (re-search-forward "name" nil t))
      (should (re-search-forward "six" nil t))
      (should (re-search-forward "version" nil t))
      (should (re-search-forward "description" nil t))
      (should (re-search-forward "Python 2 and 3 compatibility utilities" nil t)))))

(ert-deftest poetry-show-interactive-should-offer-packages ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "attrs")
    (poetry-wait-for-calls)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "attrs      4.4  Classes without ...")))
      (call-interactively 'poetry-show))
    (poetry-wait-for-calls)
    (with-current-buffer "*poetry-output*"
      (goto-char (point-min))
      (should (re-search-forward "name" nil t))
      (should (re-search-forward "attrs" nil t))
      (should (re-search-forward "description" nil t))
      (should (re-search-forward "Classes Without Boilerplate" nil t)))))

;;; poetry-show-test.el ends here
