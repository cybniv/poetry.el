;;; poetry-add-test.el --- Tests for poetry.el


(ert-deftest poetry-add-should-add-dependency ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dep "dill")
    (poetry-add-dep "unum")
    (should (string-match
             "python (.*)dill (.*)unum (.*)"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-add-should-add-dev-dependency ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-dev-dep "dill")
    (poetry-add-dev-dep "unum")
    (should (string-match
             "dill (.*)unum (.*)"
             (apply 'concat (poetry-get-dependencies t))))
    (should (string-match
             "python (.*)"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-add-should-add-opt-dependency ()
  (let* ((ppath (poetry-test-create-project-folder))
         (default-directory ppath))
    (poetry-add-opt-dep "dill")
    (poetry-add-opt-dep "unum")
    (should (string-match
             "dill (.*)unum (.*)"
             (apply 'concat (poetry-get-dependencies nil t))))
    (should (string-match
             "python (.*)"
             (apply 'concat (poetry-get-dependencies))))))

;;; poetry-add-test.el ends here
