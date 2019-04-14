;;; test-helper.el --- Helpers for poetry.el-test.el

(let ((poetry-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path poetry-dir))

;; coverage
(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'poetry)

(defun poetry-test-create-empty-folder ()
  "Create an empty temporary folder and return its path."
  (make-temp-file "poetry" t))

(defun poetry-test-create-project-folder ()
  "Create an temporary poetry project, and return its path."
  (let ((path (make-temp-file "poetry" t)))
    (poetry-new path)
    path))

;;; test-helper.el ends here
