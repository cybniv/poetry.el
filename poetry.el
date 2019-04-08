;;; poetry.el --- poetry in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019-  Gaby Launay

;; Author: Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/galaunay/poetry.el
;; Version: 0.1.0
;; Keywords: Python, Tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl)
(require 'transient)
(require 'xterm-color)

(defconst poetry-version "0.1.0"
  "Poetry.el version")


;; Transient interface
;;;###autoload
(define-transient-command poetry ()
  "Poetry menu."
  [:if poetry-find-project-root
       :description "Dependencies"
       ("a" "Add" poetry-add)
       ("r" "Remove" poetry-remove)
       ("i" "Install" poetry-install)
       ("l" "Lock" poetry-lock)
       ("u" "Update" poetry-update)
       ("s" "Show" poetry-show)]
  [["New project"
    ;; ("I" "Init" poetry-init)
    ("n" "New" poetry-new)]
   [:if poetry-find-project-root
    :description "Project"
        ("c" "Check" poetry-check)
        ("b" "Build" poetry-build)
        ("p" "Publish" poetry-publish)]]
  [[:if poetry-find-project-root
        :description "Shell"
        ("R" "Run a command" poetry-run)
        ("S" "Start a shell" poetry-shell)]
   ;; [:if poetry-find-project-root
   ;;  :description "Cache"
   ;;  ("C" "Clear" poetry-clear)]
   ["Poetry"
    ("U" "Update" poetry-self-update)]])

;; Poetry add
(define-transient-command poetry-add ()
  "Poetry add dependency menu."
  ["Arguments"
   (poetry:--git)
   (poetry:--path)
   (poetry:--python)
   (poetry:--platform)
   ]
  ["Add"
   ("a" "Add a dependency" poetry-add-dep)
   ("d" "Add a development dependency" poetry-add-dev-dep)
   ("o" "Add an optional dependency" poetry-add-opt-dep)
   ])

(define-infix-argument poetry:--git ()
  :description "Git repository"
  :class 'transient-option
  :key "-g"
  :argument "--git=")

(define-infix-argument poetry:--path ()
  :description "Dependency path"
  :class 'transient-option
  :key "-P"
  :argument "--path=")

(define-infix-argument poetry:--python ()
  :description "Python version"
  :class 'transient-option
  :key "-p"
  :argument "--python=")

(define-infix-argument poetry:--platform ()
  :description "Platforms"
  :class 'transient-option
  :key "-t"
  :argument "--platform=")

(defun poetry-call-add (package args)
  "Add PACKAGE as a new dependency to the project."
  (let ((args (concatenate 'list args
                           (transient-args 'poetry-add))))
    (poetry-call 'add nil (concatenate 'list
                                       (list package)
                                       args))))

;;;###autoload
(defun poetry-add-dep (package)
  "Add PACKAGE as a new dependency to the project."
  (interactive "sPackage name: ")
  (poetry-message (format "Adding dependency: %s" package))
  (poetry-call-add package '()))

;;;###autoload
(defun poetry-add-dev-dep (package)
  "Add PACKAGE as a new development dependency to the project."
  (interactive "sPackage name: ")
  (poetry-message (format "Adding dev dependency: %s" package))
  (poetry-call-add package '("-D")))

;;;###autoload
(defun poetry-add-opt-dep (package)
  "Add PACKAGE as a new optional dependency to the project."
  (interactive "sPackage name: ")
  (poetry-message (format "Adding optional dependency: %s" package))
  (poetry-call-add package '("--optional")))

;; Poetry remove
;;;###autoload
(defun poetry-remove (package type)
  "Remove PACKAGE from the project dependencies.

if DEV is not nil, remove a development dependency."
  (interactive (let* ((packages (concatenate 'list
                                 (map 'list
                                      (lambda (dep)
                                        (format "[dep]  %s" dep))
                                      (poetry-get-dependencies))
                                 (map 'list
                                      (lambda (dep)
                                        (format "[dev]  %s" dep))
                                      (poetry-get-dependencies t))
                                 (map 'list
                                      (lambda (dep)
                                        (format "[opt]  %s" dep))
                                      (poetry-get-dependencies nil t))))
                      (package (when packages
                                 (completing-read "Package: "
                                                  packages
                                                  nil t))))
                 (if (not package)
                     (list nil nil)
                   (string-match "^\\[\\(.*\\)\\]  \\([^[:space:]]*\\)[[:space:]]*(\\(.*\\))$" package)
                   (list (match-string 2 package)
                         (match-string 1 package)))))
  (if (not package)
      (poetry-message "No packages specified in pyproject.toml")
    (pcase type
      ("dep"
       (poetry-message (format "Removing package %s"
                               package))
       (poetry-remove-dep package))
      ("opt"
       (poetry-message (format "Removing optional package %s"
                               package))
       (poetry-remove-dep package))
      ("dev"
       (poetry-message (format "Removing development package %s"
                               package))
       (poetry-remove-dev-dep package)))))

(defun poetry-remove-dep (package)
  "Removes PACKAGE from the project dependencies."
  (interactive (list (car (split-string
                           (completing-read "Package: "
                                            (poetry-get-dependencies)
                                            nil 'confirm)
                           "[[:space:]]+"))))
  (poetry-call 'remove nil (list package)))

(defun poetry-remove-dev-dep (package)
  "Removes PACKAGE from the project development dependencies."
  (interactive (list (car (split-string
                           (completing-read "Package: "
                                            (poetry-get-dependencies t)
                                            nil 'confirm)
                           "[[:space:]]+"))))
  (poetry-call 'remove nil (list package "-D")))

;;;###autoload
(defun poetry-check ()
  "Checks the validity of the pyproject.toml file."
  (interactive)
  (poetry-call 'check t))

;;;###autoload
(defun poetry-install ()
  "Installs the project dependencies."
  (interactive)
  (poetry-call 'install))

;;;###autoload
(defun poetry-lock ()
  "Locks the project dependencies."
  (interactive)
  (poetry-call 'lock))

;;;###autoload
(defun poetry-update ()
  "Update dependencies as according to the pyproject.toml file."
  (interactive)
  (poetry-call 'update))

(defun poetry-show-get-packages ()
  "Return the list of package description for show."
  (poetry-call 'show)
  (with-current-buffer "*poetry*"
    (goto-char (point-min))
    (let (packs)
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string 1) packs))
      packs)))


;;;###autoload
(defun poetry-show (package)
  "Shows information about packages."
  (interactive
   (list (completing-read "Package: "
                          (poetry-show-get-packages))))
  (string-match "^\\([^[:space:]]*\\).*$" package)
  (poetry-call 'show t (list (match-string 1 package))))

;;;###autoload
(defun poetry-build ()
  "Builds a package, as a tarball and a wheel by default."
  (interactive)
  (poetry-call 'build))

;;;###autoload
(defun poetry-publish (repo username password)
  "Publishes a package to a remote repository."
  (interactive (list
                (completing-read "Repository: "
                                 '("pypi"))
                (read-from-minibuffer "Username: ")
                (read-passwd "Password: ")))
  (poetry-call 'publish
               (list "-r" repo "-u" username "-p" password)))

;; ;;;###autoload
;; (defun poetry-init (path)
;;   "Creates a basic pyproject.toml file at PATH."
;;   (interactive "DProject path: ")
;;   (let ((default-directory path))
;;     (poetry-call 'init)))

;;;###autoload
(defun poetry-new (path)
  "Creates a new Python project at PATH"
  (interactive "DProject path: ")
  (let ((default-directory path))
    (poetry-message (format "Creating new project: %s" path))
    (poetry-call 'new nil (list path))))

;;;###autoload
(defun poetry-run (command)
  "Runs a command in the appropriate environment."
  ;; TODO: add completion with scripts from pyptoject.toml
  (interactive "sCommand: ")
  (poetry-call 'run t (split-string command "[[:space:]]+" t)))

;;;###autoload
(defun poetry-shell ()
  "Spawns a shell within the virtual environment."
  (interactive)
  (shell "*poetry-shell*")
  (process-send-string (get-buffer-process (get-buffer "*poetry-shell*"))
                       "poetry shell\n"))

;;;###autoload
(defun poetry-clear ()
  "Clears poetry's cache."
  (interactive)
  (poetry-call 'cache:clear))

;;;###autoload
(defun poetry-self-update ()
  "Updates poetry to the latest version."
  (interactive)
  (poetry-call 'self:update))

;; Helpers
(defun poetry-call (command &optional output args)
  "Call poetry COMMAND with the given ARGS"
  (let* ((prog "poetry")
         (args (if (or (string= command "run")
                       (string= command "init"))
                   (concatenate 'list (list (symbol-name command))
                                args)
                 (concatenate 'list (list "-n" "--ansi"
                                          (symbol-name command))
                              args)))
         (poetry-buffer "*poetry*")
         error-code)
    (let ((poetry-buf (get-buffer-create poetry-buffer)))
      (with-current-buffer poetry-buf
        (delete-region (point-min) (point-max)))
      (setq error-code (apply 'call-process
                              (concatenate 'list (list prog nil
                                                       (list poetry-buf t)
                                                       t)
                                           args)))
      (with-current-buffer poetry-buf
        (xterm-color-colorize-buffer)
        (goto-char (point-min))
        (while (re-search-forward "" nil t)
          (replace-match "\n" nil nil))
        (goto-char (point-min))
        (while (re-search-forward "" nil t)
          (replace-match "" nil nil))))

    (when (or output (not (= error-code 0)))
      (poetry-display-buffer))))

;; (defun poetry-call-async (command &optional output args)
;;   "Call poetry COMMAND with the given ARGS"
;;   (let* ((command (if (or (string= command "run")
;;                           (string= command "init"))
;;                       (concatenate 'list (list "poetry" (symbol-name command))
;;                                    args)
;;                     (concatenate 'list (list "poetry" "-n" "--no-ansi"
;;                                                       (symbol-name command))
;;                                           args)))
;;         (proc (make-process :name "poetry"
;;                             :buffer " *poetry*"
;;                             :command command
;;                             :sentinel 'poetry-call-sentinel)))
;;     (process-put proc 'output output)))

;; (defun poetry-call-sentinel (process string)
;;   "Poetry call sentinel."
;;   (let ((output (process-get process 'output)))
;;     (if (not (string-match "^finished" string))
;;         (progn
;;           (poetry-display-buffer))
;;       (if output
;;           (poetry-display-buffer)))))

(defun poetry-display-buffer ()
  (with-current-buffer "*poetry*"
    (let ((buffer-read-only nil))
      ;; (xterm-color-colorize-buffer)
      ;; (goto-char (point-min))
      ;; (while (search-forward "" (point-max) t)
      ;;   (replace-match "\n"))
      (display-buffer "*poetry*"))))

(defun poetry-get-dependencies (&optional dev opt)
  "Return the list of project dependencies.

If DEV is non-nil, install a developement dep.
If OPT is non-nil, set an optional dep."
  (with-current-file (poetry-find-pyproject-file)
                     (goto-char (point-min))
                     (if dev
                         (re-search-forward "^\\[tool\\.poetry\\.dev-dependencies\\]$")
                       (re-search-forward "^\\[tool\\.poetry\\.dependencies\\]$"))
                     (let ((beg (point))
                           (end (progn (re-search-forward "^\\[")
                                       (point)))
                           (regex (if (not opt)
                                      "^\\([^= ]*\\)[[:space:]]*=[[:space:]]*\"\\(.*\\)\""
                                    "^\\([^= ]*\\)[[:space:]]*=[[:space:]]*{version[[:space:]]*=[[:space:]]*\"\\(.*\\)\"[[:space:]]*,[[:space:]]*optional[[:space:]]*=[[:space:]]*true[[:space:]]*}$"))
                           deps)
                       (goto-char beg)
                       (while (re-search-forward regex end t)
                         (push (format "%s (%s)"
                                       (substring-no-properties (match-string 1))
                                       (substring-no-properties (match-string 2)))
                               deps))
                       (reverse deps))))

(defun poetry-find-project-root ()
  "Return the poetry project root if any."
  (locate-dominating-file default-directory "pyproject.toml"))

(defun poetry-find-pyproject-file ()
  "Return the location of the 'pyproject.toml' file."
  (let ((root (poetry-find-project-root)))
    (when root
      (concat (file-name-as-directory root) "pyproject.toml"))))

(defmacro with-current-file (file &rest body)
  "Execute the forms in BODY while temporary visiting FILE."
  `(save-current-buffer
     (let* ((file ,file)
            (keep (find-buffer-visiting file))
            (buffer (find-file-noselect file)))
       (set-buffer buffer)
       (prog1
           (progn
             ,@body)
         (when (not keep)
           (kill-buffer buffer))))))

(defun poetry-get-project-name ()
  "Return the current project name."
  (let ((file (poetry-find-pyproject-file)))
    (when file
      (with-current-file file
         (goto-char (point-min))
         (when (re-search-forward "^\\[tool\\.poetry\\]$" nil t)
           (when (re-search-forward "^name = \"\\(.*\\)\"$" nil t)
             (substring-no-properties (match-string 1))))))))

(defun poetry-message (mess)
  "Display a message."
  (let ((name (poetry-get-project-name)))
    (if name
        (message "[%s] %s" name mess)
    (message "[Poetry] %s" mess))))


(provide 'poetry)
;;; poetry.el ends here
