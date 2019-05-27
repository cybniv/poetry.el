;;; poetry-add-test.el --- Tests for poetry.el


(ert-deftest poetry-add-should-add-dependency ()
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (should (string-match
             "^python (.*)atomicwrites (.*)attrs (.*)$"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-add-should-add-dev-dependency ()
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dev-dep "atomicwrites")
    (poetry-add-dev-dep "attrs")
    (should (string-match
             "^pytest (.*)atomicwrites (.*)attrs (.*)$"
             (apply 'concat (poetry-get-dependencies t))))
    (should (string-match
             "python (.*)"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-add-should-add-opt-dependency ()
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-opt-dep "atomicwrites")
    (poetry-add-opt-dep "attrs")
    (should (string-match
             "^atomicwrites (.*)attrs (.*)$"
             (apply 'concat (poetry-get-dependencies nil t))))
    (should (string-match
             "^python (.*)$"
             (apply 'concat (poetry-get-dependencies))))))

;;; poetry-add-test.el ends here
