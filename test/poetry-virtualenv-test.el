;;; poetry-virtualenv-test.el --- Tests for poetry.el


(ert-deftest poetry-virtualenv-should-be-activated-and-deactivated ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (project-path (file-name-directory
                          (directory-file-name
                           (file-name-directory ppath))))
           (ppath2 (poetry-test-create-project-folder))
           (project-path2 (file-name-directory
                           (directory-file-name
                            (file-name-directory ppath2)))))
      (find-file ppath)
      (poetry-add-dep "atomicwrites")
      (poetry-wait-for-calls)
      ;; should activate the venv
      (poetry-venv-workon)
      (should (string-match (concat project-path ".venv") pyvenv-virtual-env))
      ;; should deactivate the venv
      (poetry-venv-deactivate)
      (should (not pyvenv-virtual-env))
      ;; should activate with 'toggle'
      (poetry-venv-toggle)
      (should (string-match (concat project-path ".venv") pyvenv-virtual-env))
      ;; should deactivate with toggle
      (poetry-venv-toggle)
      (should (not pyvenv-virtual-env))
      ;; 'toggle' should activate even if there is another venv activated
      (poetry-venv-workon)
      (find-file ppath2)
      (poetry-venv-toggle)
      (should (string-match (concat project-path2 ".venv")
                            pyvenv-virtual-env)))))

(ert-deftest poetry-virtualenv-should-error ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let ((ppath (poetry-test-create-project-folder))
          (ppath2 (poetry-test-create-project-folder)))
      (find-file ppath)
      ;; try to deactivate when nothing activated
      (should-error (poetry-venv-deactivate))
      (poetry-venv-workon)
      ;; try to deactivate when the current venv is another venv
      (find-file ppath2)
      (should-error (poetry-venv-deactivate)))))

(ert-deftest poetry-should-track-virtualenvs ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (project-path (file-name-directory
                          (directory-file-name
                           (file-name-directory ppath))))
           (ppath2 (poetry-test-create-project-folder))
           (project-path2 (file-name-directory
                          (directory-file-name
                           (file-name-directory ppath2))))
           (not-project-path (make-temp-file "poetry-not-project")))
      (find-file ppath)
      (poetry-add-dep "atomicwrites")
      (poetry-wait-for-calls)
      (find-file ppath2)
      (poetry-add-dep "attrs")
      (poetry-wait-for-calls)
      (find-file not-project-path)
      (poetry-tracking-mode 1)
      ;; project 1
      (find-file (concat (file-name-as-directory ppath)
                         "file1.py"))
      (run-hooks 'post-command-hook)
      (should (string-match (concat project-path ".venv") pyvenv-virtual-env))
      ;; project 2
      (find-file (concat (file-name-as-directory ppath2)
                         "file1.py"))
      (run-hooks 'post-command-hook)
      (should (string-match (concat project-path2 ".venv") pyvenv-virtual-env))
      ;; directory 3 (not poetry)
      (find-file not-project-path)
      (run-hooks 'post-command-hook)
      (should (not pyvenv-virtual-env))
      ;;
      (poetry-tracking-mode -1))))

(ert-deftest poetry-should-restore-previous-venv-when-leaving-project-files ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (project-path (file-name-directory
                          (directory-file-name
                           (file-name-directory ppath))))
           (not_ppath (make-temp-file "poetry_not_python"))
           (pyvenv-virtual-env "/path/to/global/venv"))
      ;; not in a project, venv should be global
      (find-file not_ppath)
      (poetry-tracking-mode 1)
      (should (string-match "/path/to/global/venv" pyvenv-virtual-env))
      ;; in a project, venv should be the project one
      (find-file ppath)
      (run-hooks 'post-command-hook)
      (should (string-match (concat project-path ".venv")
                            pyvenv-virtual-env))
      ;; leaving the project, venv should be restored to global
      (find-file not_ppath)
      (run-hooks 'post-command-hook)
      (should (string-match "/path/to/global/venv" pyvenv-virtual-env)))))

(ert-deftest poetry-should-deactivate-venv-when-deactivating-tracking ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (project-path (file-name-directory
                          (directory-file-name
                           (file-name-directory ppath))))
           (not_ppath (make-temp-file "poetry_not_python")))
      ;; not in a project, venv should be nil
      (find-file not_ppath)
      (poetry-tracking-mode 1)
      (should (not pyvenv-virtual-env))
      ;; in a project, venv should be the project one
      (find-file ppath)
      (run-hooks 'post-command-hook)
      (should (string-match (concat project-path ".venv")
                            pyvenv-virtual-env))
      ;; deactivating tracking mode should remove the venv
      (poetry-tracking-mode -1)
      (should (not pyvenv-virtual-env)))))

(ert-deftest poetry-should-restore-previous-venv-when-deactivating-tracking ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let* ((ppath (poetry-test-create-project-folder))
           (project-path (file-name-directory
                          (directory-file-name
                           (file-name-directory ppath))))
           (not_ppath (make-temp-file "poetry_not_python"))
           (pyvenv-virtual-env "/path/to/global/venv"))
      ;; not in a project, venv should be global
      (find-file not_ppath)
      (poetry-tracking-mode 1)
      (should (string-match "/path/to/global/venv" pyvenv-virtual-env))
      ;; in a project, venv should be the project one
      (find-file ppath)
      (run-hooks 'post-command-hook)
      (should (string-match (concat project-path ".venv")
                            pyvenv-virtual-env))
      ;; deactivating tracking mode should restore the global venv
      (poetry-tracking-mode -1)
      (should (string-match "/path/to/global/venv" pyvenv-virtual-env)))))

(ert-deftest poetry-venv-exist-should-detect-venv ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'poetry-get-configuration)
             (lambda (key)
               (cond
                ((string= key "virtualenvs.in-project")
                 nil)
                ((string= key "virtualenvs.path")
                 "/tmp/venv-test")
                nil))))
    (let* ((ppath (poetry-test-create-project-folder)))
      (find-file ppath)
      (poetry-add-dep "atomicwrites")
      (poetry-wait-for-calls)
      (mkdir (format "/tmp/venv-test/%s-foobar-py3.8"
                     (poetry-get-project-name))
             t)
      (should (poetry-venv-exist-p))
      (delete-directory (poetry-get-virtualenv) t)
      (should (not (poetry-venv-exist-p))))))

(ert-deftest poetry-venv-activated-should-detect-activated-venv ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let ((ppath (poetry-test-create-project-folder)))
      (find-file ppath)
      (poetry-add-dep "atomicwrites")
      (poetry-wait-for-calls)
      (should (not (poetry-venv-activated-p)))
      (poetry-venv-workon)
      (should (poetry-venv-activated-p)))))

(ert-deftest poetry-should-error-when-trying-to-activate-venv-with-tracking ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'pyvenv-activate)
             (lambda (dir)
               (setq pyvenv-virtual-env dir)))
            ((symbol-function 'directory-files)
             (lambda (dir &optional full match nosort)
               (list (concat (file-name-as-directory dir)
                             (poetry-normalize-project-name (poetry-get-project-name)))))))
    (let ((ppath (poetry-test-create-project-folder)))
      (find-file ppath)
      (poetry-add-dep "atomicwrites")
      (poetry-wait-for-calls)
      (poetry-tracking-mode 1)
      (should-error (poetry-venv-workon))
      (should-error (poetry-venv-deactivate)))))

(ert-deftest poetry-should-check-for-poetry-config ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'poetry-get-configuration)
             (lambda (key)
               (cond
                ((string= key "virtualenvs.in-project")
                 t)
                ((string= key "virtualenvs.path")
                 "/tmp/venv-test")
                nil))))
    (let ((ppath (poetry-test-create-project-folder)))
      (find-file ppath)
      (poetry-wait-for-calls)
      (should (string= (poetry-get-virtualenv)
                       (concat (file-name-as-directory
                                (poetry-find-project-root))
                               ".venv"))))))

(ert-deftest poetry-should-check-for-poetry-config-2 ()
  (poetry-test-cleanup)
  (cl-letf (((symbol-function 'poetry-get-configuration)
             (lambda (key)
               (cond
                ((string= key "virtualenvs.in-project")
                 nil)
                ((string= key "virtualenvs.path")
                 "/tmp/venv-test")
                nil))))
    (let ((ppath (poetry-test-create-project-folder)))
      (make-directory (format "/tmp/venv-test/%s"
                              (format "%s-foobar-py3.8"
                                      (poetry-get-project-name)))
                      t)
      (find-file ppath)
      (poetry-wait-for-calls)
      (should (string-match
               "/tmp/venv-test"
               (poetry-get-virtualenv))))))


;;; poetry-virtualenv-test.el ends here
