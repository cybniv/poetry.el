;;; poetry-remove-test.el --- Tests for poetry.el


(ert-deftest poetry-remove-should-remove-dependency ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-remove "atomicwrites" "dep")
    (poetry-remove "attrs" "dep")
    (should (string-match
             "^python (.*)$"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-remove-should-remove-dev-dependency ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dev-dep "atomicwrites")
    (poetry-add-dev-dep "attrs")
    (poetry-remove "atomicwrites" "dev")
    (poetry-remove "attrs" "dev")
    (should (string-match
             "^pytest (.*)$"
             (apply 'concat (poetry-get-dependencies t))))
    (should (string-match
             "^python (.*)$"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-remove-should-remove-opt-dependency ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-remove "atomicwrites" "opt")
    (poetry-remove "attrs" "opt")
    (should (string-match
             "^$"
             (apply 'concat (poetry-get-dependencies nil t))))
    (should (string-match
             "^python (.*)$"
             (apply 'concat (poetry-get-dependencies))))))

;;; poetry-remove-test.el ends here
