;;; poetry-show-test.el --- Tests for poetry.el

(ert-deftest poetry-show-should-display-packages ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (should (string-match
             (concat
              "six[[:space:]]+[0-9.]+ .*"
              "pytest[[:space:]]+[0-9.]+ .*"
              "py[[:space:]]+[0-9.]+ .*"
              "pluggy[[:space:]]+[0-9.]+ .*"
              "more-itertools[[:space:]]+[0-9.]+ .*"
              "attrs[[:space:]]+[0-9.]+ .*"
              "atomicwrites[[:space:]]+[0-9.]+ .*")
             (substring-no-properties
              (apply 'concat (poetry-show-get-packages)))))))

(ert-deftest poetry-show-should-display-package-info ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-show (car (poetry-show-get-packages)))
    (should (with-current-buffer "*poetry*"
              (goto-char (point-min))
              (re-search-forward "^name[[:space:]]*: six$" nil t)
              (re-search-forward "description[[:space:]]*: Python 2 and 3 compatibility utilities" nil t)))))

;;; poetry-show-test.el ends here
