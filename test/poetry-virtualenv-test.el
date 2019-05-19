;;; poetry-virtualenv-test.el --- Tests for poetry.el


(ert-deftest poetry-virtualenv-should-be-activated-and-deactivated ()
  (when (not (getenv "TRAVIS"))
    (let* ((ppath (poetry-test-create-project-folder))
           (default-directory ppath))
      (poetry-add-dep "atomicwrites")
      ;; (poetry-add-dep "attrs")
      (poetry-venv-workon)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      (poetry-venv-deactivate)
      (should (not pyvenv-virtual-env))
      (poetry-venv-toggle)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      (poetry-venv-toggle)
      (should (not pyvenv-virtual-env)))))

(ert-deftest poetry-should-track-virtualenvs ()
  (when (not (getenv "TRAVIS"))
    (let* ((ppath (poetry-test-create-project-folder))
           (ppath2 (poetry-test-create-project-folder)))
      (let ((default-directory ppath))
        (poetry-add-dep "atomicwrites"))
      (let ((default-directory ppath2))
        (poetry-add-dep "attrs"))
      (poetry-tracking-mode 1)
      ;; project 1
      (find-file (concat (file-name-as-directory ppath)
                         "file1.py"))
      (run-hooks 'post-command-hook)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      ;; project 2
      (find-file (concat (file-name-as-directory ppath)
                         "file1.py"))
      (run-hooks 'post-command-hook)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      (poetry-tracking-mode -1)
      (poetry-venv-deactivate))))

;;; poetry-virtualenv-test.el ends here
