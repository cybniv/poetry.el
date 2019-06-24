;;; poetry-remove-test.el --- Tests for poetry.el


(ert-deftest poetry-remove-should-remove-dependency ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-remove "atomicwrites" "dep")
    (poetry-remove "attrs" "dep")
    (poetry-wait-for-calls)
    (should (not (string-match
             "^attrs$"
             (apply 'concat (poetry-get-dependencies nil t)))))
    (should (not (string-match
             "^atomicwrites$"
             (apply 'concat (poetry-get-dependencies nil t)))))))

(ert-deftest poetry-remove-should-remove-dev-dependency ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dev-dep "atomicwrites")
    (poetry-add-dev-dep "attrs")
    (poetry-remove "atomicwrites" "dev")
    (poetry-remove "attrs" "dev")
    (poetry-wait-for-calls)
    (should (not (string-match
             "^attrs$"
             (apply 'concat (poetry-get-dependencies nil t)))))
    (should (not (string-match
             "^atomicwrites$"
             (apply 'concat (poetry-get-dependencies nil t)))))))

(ert-deftest poetry-remove-should-remove-opt-dependency ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-opt-dep "atomicwrites")
    (poetry-add-opt-dep "attrs")
    (poetry-remove "atomicwrites" "opt")
    (poetry-remove "attrs" "opt")
    (poetry-wait-for-calls)
    (should (not (string-match
             "^attrs$"
             (apply 'concat (poetry-get-dependencies nil t)))))
    (should (not (string-match
             "^atomicwrites$"
             (apply 'concat (poetry-get-dependencies nil t)))))))

(ert-deftest poetry-remove-interactive-should-propose-package-list ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dev-dep "attrs")
    (poetry-add-opt-dep "six")
    (poetry-wait-for-calls)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "[dev]  attrs (^4.4)")))
      (call-interactively 'poetry-remove))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "[dep]  atomicwrites (^4.4)")))
      (call-interactively 'poetry-remove))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) "[opt]  six (^4.4)")))
      (call-interactively 'poetry-remove))
    (poetry-wait-for-calls)
    (should (not (string-match
             "^atomicwrites (.*)$"
             (apply 'concat (poetry-get-dependencies t)))))
    (should (not (string-match
             "^attrs (.*)$"
             (apply 'concat (poetry-get-dependencies t)))))
    (should (not (string-match
             "^six (.*)$"
             (apply 'concat (poetry-get-dependencies t)))))))

(ert-deftest poetry-remove-interactive-should-error-when-nothing-to-remove ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (should-error
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest ignore) nil)))
      (call-interactively 'poetry-remove)))))


;;; poetry-remove-test.el ends here
