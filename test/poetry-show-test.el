;;; poetry-show-test.el --- Tests for poetry.el

(ert-deftest poetry-show-should-display-packages ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (should (string-match
             "six            .* Python 2 and 3 compatibility utilitiespytest         .* pytest: simple powerful testing with Pythonpy             .*  library with cross-python path, ini-parsing, io, code...pluggy         .*  plugin and hook calling mechanisms for pythonmore-itertools .*  More routines for operating on iterables, beyond iter...attrs          .* Classes Without Boilerplateatomicwrites   .*  Atomic file writes."
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
