;;; poetry.el --- Interface to Poetry -*- lexical-binding: t -*-

;; Copyright (C) 2019-  Gaby Launay

;; Author: Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/galaunay/poetry.el
;; Keywords: Python, Tools
;; Package-Version: 0.1.0
;; Package-Requires: ((transient "0.1.0") (xterm-color "1.8") (pyvenv "1.2") (emacs "25.1"))

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


;;; Commentary:

;; This package offers an interface to poetry (https://poetry.eustace.io/),
;; a Python dependency management and packaging command line tool.

;; Poetry.el uses transient to provide a magit-like interface. The
;; entry point is simply: `poetry'

;; Poetry.el also provides a global minor mode that automatically
;; activates the associated virtualenv when visiting a poetry project.
;; You can activate this feature with `poetry-tracking-mode'.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'xterm-color)
(require 'pyvenv)

;; Variables
;;;;;;;;;;;;

(defgroup poetry nil
  "Poetry in Emacs."
  :prefix "poetry-"
  :group 'tools)

(defcustom poetry-virtuelenv-path
  (cond
   ((or (eq system-type 'ms-dos)
        (eq system-type 'windows-nt))
    (expand-file-name "%APPDATA%/Local/pypoetry/Cache/virtualenvs"))
   ((eq system-type 'darwin)
    (expand-file-name "~/Library/Caches/pypoetry/virtualenvs"))
   (t
    (expand-file-name "~/.cache/pypoetry/virtualenvs")))
  "Path to poetry virtualenvs directory."
  :type 'string)

(defcustom poetry-repository-list '("pypi")
  "List of repository name to register package to."
  :type '(repeat string))


;; Macros
;;;;;;;;;

(defmacro poetry-with-current-file (file &rest body)
  "Execute the forms in BODY while temporary visiting FILE."
  `(save-current-buffer
     (let* ((file ,file)
            (keep (find-buffer-visiting file))
            (buffer (find-file-noselect file)))
       (set-buffer buffer)
       (prog1
           (progn
             ,@body)
         (unless keep
           (kill-buffer buffer))))))


;; Transient interface
;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-transient-command poetry ()
  "Poetry menu."
  [:description (lambda () (format "Project: %s\n" (poetry-get-project-name)))
  [:if poetry-find-project-root
       :description "Dependencies"
       ("a" "Add" poetry-add)
       ("r" "Remove" poetry-remove)
       ("i" "Install" poetry-install)
       ("l" "Lock" poetry-lock)
       ("u" "Update" poetry-update)
       ("s" "Show" poetry-show)]]
  [:if poetry-find-project-root
       :description "Virtualenv"
       ("v" "Toggle virtualenv" poetry-venv-toggle)]
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


;; Poetry functions
;;;;;;;;;;;;;;;;;;;

(defun poetry-call-add (package args)
  "Add PACKAGE as a new dependency to the project.

ARGS are additionnal arguments passed to ``poetry add''."
  (let ((args (cl-concatenate 'list args
                           (transient-args 'poetry-add))))
    (poetry-call 'add nil (cl-concatenate 'list
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

;;;###autoload
(defun poetry-remove (package type)
  "Remove PACKAGE from the project dependencies.

TYPE is the type of dependency (dep, dev or opt)."
  (interactive (let* ((packages (cl-concatenate 'list
                                 (cl-map 'list
                                      (lambda (dep)
                                        (format "[dep]  %s" dep))
                                      (poetry-get-dependencies))
                                 (cl-map 'list
                                      (lambda (dep)
                                        (format "[dev]  %s" dep))
                                      (poetry-get-dependencies t))
                                 (cl-map 'list
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
      (poetry-error "No packages to remove")
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
  "Remove PACKAGE from the project dependencies."
  (poetry-call 'remove nil (list package)))

(defun poetry-remove-dev-dep (package)
  "Remove PACKAGE from the project development dependencies."
  (poetry-call 'remove nil (list package "-D")))

;;;###autoload
(defun poetry-check ()
  "Check the validity of the pyproject.toml file."
  (interactive)
  (poetry-call 'check t))

;;;###autoload
(defun poetry-install ()
  "Install the project dependencies."
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
  "Show information about package PACKAGE."
  (interactive
   (list (completing-read "Package: "
                          (poetry-show-get-packages))))
  (string-match "^\\([^[:space:]]*\\).*$" package)
  (poetry-call 'show t (list (match-string 1 package))))

;;;###autoload
(defun poetry-build ()
  "Build a package, as a tarball and a wheel by default."
  (interactive)
  (poetry-call 'build))

;;;###autoload
(defun poetry-publish (repo username password)
  "Publish the package to a remote repository.

REPO is the repository and USERNAME and PASSWORD the
credential to use."
  (interactive (list
                (completing-read "Repository: "
                                 poetry-repository-list)
                (read-string "Username: ")
                (read-passwd "Password: ")))
  (poetry-call 'publish
               (list "-r" repo "-u" username "-p" password)))

;;;###autoload
(defun poetry-new (path)
  "Create a new Python project at PATH."
  (interactive "GProject path: ")
  (let* ((path (expand-file-name path))
         (project-name (file-name-base path))
         (default-directory path))
    (poetry-message (format "Creating new project: %s" path))
    (unless (file-directory-p path)
      (make-directory path))
    (poetry-call 'new nil (list path))
    ;; Open __init__.py
    (find-file (concat (file-name-as-directory
                        (concat (file-name-as-directory path)
                                (replace-regexp-in-string
                                 "-"
                                 "_"
                                 (downcase project-name))))
                        "__init__.py"))
    ;; make sure the virtualenv is created
    (poetry-call 'run nil (split-string "python -V" "[[:space:]]+" t))))

;;;###autoload
(defun poetry-run (command)
  "Run COMMAND in the appropriate environment."
  (interactive (list (completing-read "Command: "
           (let* ((file (poetry-find-pyproject-file))
                  (scripts '()))
             (when file
               (poetry-with-current-file file
                (goto-char (point-min))
                (when (re-search-forward
                       "^\\[tool\\.poetry\\.scripts\\]$" nil t)
                  (forward-line 1)
                  (beginning-of-line)
                  (while (re-search-forward
                          "^\\([^=]+\\)[[:space:]]*=[[:space:]]*\".*\"$"
                          (line-end-position) t)
                    (push (substring-no-properties (match-string 1)) scripts)
                    (forward-line)
                    (beginning-of-line)))))
             scripts))))
  (poetry-ensure-in-project)
  (poetry-call 'run t (split-string command "[[:space:]]+" t)))

;;;###autoload
(defun poetry-shell ()
  "Spawn a shell within the virtual environment."
  (interactive)
  (poetry-ensure-in-project)
  (shell "*poetry-shell*")
  (process-send-string (get-buffer-process (get-buffer "*poetry-shell*"))
                       "poetry shell\n"))

;;;###autoload
(defun poetry-self-update ()
  "Update poetry to the latest version."
  (interactive)
  (poetry-call 'self:update))


;; Virtualenv support
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun poetry-venv-workon ()
  "Activate the virtualenv associated to the current poetry project."
  (interactive)
  (poetry-ensure-in-project)
  (pyvenv-activate (poetry-get-virtualenv)))

;;;###autoload
(defun poetry-venv-deactivate ()
  "De-activate the virtualenv associated to the current poetry project."
  (interactive)
  (let ((venv (poetry-get-virtualenv)))
    (if (not pyvenv-virtual-env)
        (poetry-error "No virtualenv activated")
      (if (not (equal (file-name-as-directory venv)
                      (file-name-as-directory pyvenv-virtual-env)))
          (poetry-error "Current poetry virtualenv not activated")
        (pyvenv-deactivate)))))

;;;###autoload
(defun poetry-venv-toggle ()
  "Toggle the virtualenv associated to the current poetry project."
  (interactive)
  (let ((venv (poetry-get-virtualenv)))
    (if (not pyvenv-virtual-env)
        (poetry-venv-workon)
      (if (equal (file-name-as-directory venv)
                 (file-name-as-directory pyvenv-virtual-env))
          (poetry-venv-deactivate)
        (poetry-venv-workon)))))


;; Virtualenv tracking
;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode poetry-tracking-mode
  "Global minor mode to track the current poetry virtualenv.

For every project, Poetry automatically creates a virtualenv and install the project dependencies in it.
This minor mode automatically activates the relevant Poetry virtualenv when visiting a project file.

It ensures that your python scripts are always executed in the right environment."
  :global t
  :group 'poetry
  (if poetry-tracking-mode
      (add-hook 'post-command-hook 'poetry-track-virtualenv)
    (remove-hook 'post-command-hook 'poetry-track-virtualenv)))

(defvar poetry-saved-venv nil
  "Virtualenv activated before poetry.el changed it.

Allow to re-enable the previous virtualenv when leaving the poetry project.")

(defvar poetry-venv-list '()
  "List of known poetry virtualenvs.")

(defun poetry-track-virtualenv ()
  "Automatically activate virtualenvs when visiting a poetry project."
  (cond
   ;; If in a poetry project, activate the associated virtualenv
   ((poetry-find-project-root)
    (let ((poetry-venv (poetry-get-virtualenv)))
      (when (and poetry-venv
                 (not (equal (file-name-as-directory poetry-venv)
                             pyvenv-virtual-env)))
        ;; Save previous virtualenv
        (when (and pyvenv-virtual-env
                   (not (member (file-name-as-directory pyvenv-virtual-env)
                                poetry-venv-list)))
          (setq poetry-saved-venv pyvenv-virtual-env))
        (add-to-list 'poetry-venv-list (file-name-as-directory poetry-venv))
        (pyvenv-activate poetry-venv))))
   ;; If not in a poetry project, deactivate the poetry virtualenv
   ((and pyvenv-virtual-env
         (member (file-name-as-directory pyvenv-virtual-env) poetry-venv-list))
    (if (not poetry-saved-venv)
        (pyvenv-deactivate)
      (pyvenv-activate poetry-saved-venv)
      (setq poetry-saved-venv nil)))))


;; Helpers
;;;;;;;;;;

(defun poetry-call (command &optional output args)
  "Call poetry COMMAND with the given ARGS.

if OUTPUT is non-nil, display the poetry buffer when finished."
  (unless (member command '(new update))
    (poetry-ensure-in-project))
  (let* ((prog (or (executable-find "poetry")
                   (poetry-error "Could not find 'poetry' executable")))
         (args (if (or (string= command "run")
                       (string= command "init"))
                   (cl-concatenate 'list (list (symbol-name command))
                                args)
                 (cl-concatenate 'list (list "-n" "--ansi"
                                          (symbol-name command))
                              args)))
         (poetry-buffer "*poetry*")
         error-code)
    (let ((poetry-buf (get-buffer-create poetry-buffer)))
      (with-current-buffer poetry-buf
        (delete-region (point-min) (point-max)))
      (setq error-code (apply #'call-process
                              (cl-concatenate 'list (list prog nil
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

    (unless (= error-code 0)
      (poetry-display-buffer)
      (poetry-error "Error while running a poetry command. Check `*poetry*' buffer for more information"))
    (when output
      (poetry-display-buffer))))

(defun poetry-display-buffer ()
  "Display the poetry buffer."
  (with-current-buffer "*poetry*"
    (let ((buffer-read-only nil))
      (display-buffer "*poetry*"))))

(defun poetry-get-dependencies (&optional dev opt)
  "Return the list of project dependencies.

If DEV is non-nil, install a developement dep.
If OPT is non-nil, set an optional dep."
  (poetry-with-current-file (poetry-find-pyproject-file)
     (goto-char (point-min))
     (if dev
         (unless
             (re-search-forward "^\\[tool\\.poetry\\.dev-dependencies\\]$"
                                nil t)
           (poetry-error "No dependencies to remove"))
       (unless
              (re-search-forward "^\\[tool\\.poetry\\.dependencies\\]$"
                                 nil t)
         (poetry-error "No dependencies to remove")))
     (let ((beg (point))
           (end (progn (re-search-forward "^\\[" nil t)
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

(defvar-local poetry-project-name nil
  "Name of the current poetry project.")

(defvar-local poetry-project-root nil
  "Path to the current poetry project root.")

(defvar-local poetry-project-venv nil
  "Path of the virtualenv associated to the poetry project.")

(defun poetry-get-project-name ()
  "Return the current project name."
  (or poetry-project-name
      (setq poetry-project-name
            (let ((file (poetry-find-pyproject-file)))
              (when file
                (poetry-with-current-file file
                   (goto-char (point-min))
                   (when (re-search-forward "^\\[tool\\.poetry\\]$" nil t)
                     (when (re-search-forward "^name = \"\\(.*\\)\"$" nil t)
                       (substring-no-properties (match-string 1))))))))))

;;;###autoload
(defun poetry-find-project-root ()
  "Return the poetry project root if any."
  (or poetry-project-root
      (setq poetry-project-root
            (locate-dominating-file default-directory "pyproject.toml"))))

(defun poetry-get-virtualenv ()
  "Return the current poetry project virtualenv."
  (poetry-ensure-in-project)
  (or poetry-project-venv
      (setq poetry-project-venv
            (or
             (let ((poetry-project-name (poetry-get-project-name)))
              (car (directory-files
                     poetry-virtuelenv-path
                     t
                     (format "%s-py" (downcase poetry-project-name)))))
             (poetry-error "No virtualenv associated to this project")))))

(defun poetry-find-pyproject-file ()
  "Return the location of the 'pyproject.toml' file."
  (let ((root (poetry-find-project-root)))
    (when root
      (concat (file-name-as-directory root) "pyproject.toml"))))

(defun poetry-ensure-in-project ()
  "Return an error if not in a poetry project."
  (unless (poetry-find-project-root)
    (poetry-error "Not in a poetry project")))

(defun poetry-message (mess)
  "Display the message MESS."
  (message "[%s] %s" (or (poetry-get-project-name) "Poetry") mess))

(defun poetry-error (mess)
  "Display the error MESS."
  (error "[%s] %s" (or (poetry-get-project-name) "Poetry") mess))


(provide 'poetry)
;;; poetry.el ends here
