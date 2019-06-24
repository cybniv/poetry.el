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
    (poetry-wait-for-calls)
    (find-file "dummy-file.py")
    (save-buffer)
    path))

(defun poetry-wait-for-calls ()
  "Wait until all the current calls are done."
    (while (or (poetry--busy-p)
               (/= (length poetry-call-queue) 0))
               (sleep-for .1)))

(defun poetry-kill-all-buffers ()
  "Kill all poetry buffers."
  (poetry-wait-for-calls)
  (let ((kill-buffer-query-functions '()))
  (dolist (buff (buffer-list))
          (when (string-match "\\*poetry" (buffer-name buff))
            (kill-buffer buff)))))

(defun poetry-test-cleanup ()
  (poetry-kill-all-buffers))

;;; test-helper.el ends here
