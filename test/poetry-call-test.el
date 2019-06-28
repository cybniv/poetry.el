;;; poetry-call-test.el --- Tests for poetry.el

(ert-deftest poetry-call-should-error-when-no-executable ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-empty-folder))
        (exec-path '()))
    (should-error (poetry-new ppath))))

(ert-deftest poetry-call-should-queue-calls ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-add-dep "six")
    (should (/= (length poetry-call-queue) 0))
    (poetry-wait-for-calls)
    (should (= (length poetry-call-queue) 0))))

(ert-deftest poetry-call-should-continue-queue-calls-when-error ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-add-dep "attrs")  ; generate an error
    (poetry-add-dep "six")
    (should (/= (length poetry-call-queue) 0))
    (poetry-wait-for-calls)
    (should (= (length poetry-call-queue) 0))))

(ert-deftest poetry-call-should-handle-changing-project ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder))
        (ppath2 (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (find-file ppath2)
    (poetry-wait-for-calls)
    ;; nothing should have been added to the second project
    (should (string-match
             "^python (.*)$"
             (apply 'concat (poetry-get-dependencies))))
    ;; dependencies should have been added to the second project
    (find-file ppath)
    (should (string-match
             "^python (.*)atomicwrites (.*)attrs (.*)$"
             (apply 'concat (poetry-get-dependencies))))))

(ert-deftest poetry-call-should-handle-blocking-call-at-the-end-of-a-queue ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "attrs")
    (poetry-show "attrs")
    ;; nothing should remains on the queue
    (should (and (= (length poetry-call-queue) 0)
                 (not (poetry--busy-p))))))

(ert-deftest poetry-output-buffers-should-be-reused ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "six")
    (poetry-show "atomicwrites")
    (poetry-show "six")
    (poetry-wait-for-calls)
    (with-current-buffer "*poetry-output*"
      (goto-char (point-min))
      (should (re-search-forward "name" nil t))
      (should (re-search-forward "six" nil t))
      (should (re-search-forward "version" nil t))
      (should (re-search-forward "description" nil t))
      (should (re-search-forward "Python 2 and 3 compatibility utilities" nil t)))))

(ert-deftest poetry-error-buffers-should-be-reused ()
  (poetry-test-cleanup)
  (let ((ppath (poetry-test-create-project-folder)))
    (find-file ppath)
    (poetry-add-dep "atomicwrites")
    (poetry-add-dep "atomicwrites")
    (poetry-wait-for-calls)
    (with-current-buffer "*poetry-error*"
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "\nFIRST ERROR")))
    (poetry-add-dep "six")
    (poetry-add-dep "six")
    (poetry-wait-for-calls)
    (should (with-current-buffer "*poetry-error*"
                (goto-char (point-min))
                (not (re-search-forward "FIRST ERROR" nil t))))))


;;; poetry-call-test.el ends here
