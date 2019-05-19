;;; poetry-virtualenv-test.el --- Tests for poetry.el


(ert-deftest poetry-virtualenv-should-be-activated-and-deactivated ()
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (format "%s%s" (file-name-as-directory dir)
                             (downcase (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (ppath2 (poetry-test-create-project-folder))
           (default-directory ppath))
      (poetry-add-dep "atomicwrites")
      ;; should activate the venv
      (poetry-venv-workon)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      ;; should deactivate the venv
      (poetry-venv-deactivate)
      (should (not pyvenv-virtual-env))
      ;; should activate with 'toggle'
      (poetry-venv-toggle)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      ;; should deactivate with toggle
      (poetry-venv-toggle)
      (should (not pyvenv-virtual-env))
      ;; 'toggle' should activate even if there is another venv activated
      (poetry-venv-workon)
      (let ((default-directory ppath2)
            (poetry-project-venv nil)
            (poetry-project-name nil)
            (poetry-project-root nil))
        (poetry-venv-toggle)
        (should (string-match "pypoetry/virtualenvs/poetry"
                              pyvenv-virtual-env))))))

(ert-deftest poetry-virtualenv-should-error ()
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (format "%s%s" (file-name-as-directory dir)
                             (downcase (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (ppath2 (poetry-test-create-project-folder))
           (default-directory ppath))
      ;; try to deactivate when nothing activated
      (should-error (poetry-venv-deactivate))
      (poetry-venv-workon)
      ;; try to deactivate when the current venv is another venv
      (let (default-directory ppath2)
        (should-error (poetry-venv-deactivate))))))

(ert-deftest poetry-should-track-virtualenvs ()
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (format "%s%s" (file-name-as-directory dir)
                             (downcase (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (ppath2 (poetry-test-create-project-folder)))
      (let ((default-directory ppath))
        (poetry-add-dep "atomicwrites"))
      (let ((default-directory ppath2))
        (poetry-add-dep "attrs"))
      (poetry-tracking-mode 1)
      ;; project 1
      (message "Project1")
      (find-file (concat (file-name-as-directory ppath)
                         "file1.py"))
      (run-hooks 'post-command-hook)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      ;; project 2
      (message "Project2")
      (find-file (concat (file-name-as-directory ppath2)
                         "file1.py"))
      (run-hooks 'post-command-hook)
      (should (string-match "pypoetry/virtualenvs/poetry" pyvenv-virtual-env))
      ;; directory 3 (not poetry)
      (message "File3")
      (switch-to-buffer "*scratch*")
      (run-hooks 'post-command-hook)
      (should (not pyvenv-virtual-env))
      ;;
      (poetry-tracking-mode -1))))

(ert-deftest poetry-should-restore-previous-venv-when-leaving ()
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (format "%s%s" (file-name-as-directory dir)
                             (downcase (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (pyvenv-virtual-env "/path/to/global/venv"))
      (poetry-tracking-mode 1)
      ;; venv should be global
      (should (string-match "/path/to/global/venv" pyvenv-virtual-env))
      (let ((default-directory ppath))
        (run-hooks 'post-command-hook)
        ;; venv should be poetry one
        (should (string-match "pypoetry/virtualenvs/poetry"
                              pyvenv-virtual-env)))
      ;; venv should be be restored to global
      (run-hooks 'post-command-hook)
      (should (string-match "/path/to/global/venv" pyvenv-virtual-env)))))


;;; poetry-virtualenv-test.el ends here
