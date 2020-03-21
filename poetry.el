;;; poetry.el --- Interface to Poetry -*- lexical-binding: t -*-

;; Copyright (C) 2019-  Gaby Launay

;; Author: Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/galaunay/poetry.el
;; Keywords: Python, Tools
;; Package-Version: 0.1.0
;; Package-Requires: ((transient "0.1.0") (pyvenv "1.2") (emacs "25.1"))

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


;;; Todos:

;;  - Add missing Poety commands ?
;;  - Get 'publish' candidates from Poetry config
;;  - Make a 'getting started' section in the readme

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'pyvenv)
(require 'subr-x)


;; Variables
;;;;;;;;;;;;


(defgroup poetry nil
  "Poetry in Emacs."
  :prefix "poetry-"
  :group 'tools)

(defcustom poetry-virtualenv-path
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

(make-obsolete-variable 'poetry-virtualenv-path
                        "Poetry.el now reads the virtualenvs path from Poetry settings. Please use `poetry config` to change the virtualenvs path."
                        nil "0.2.0")

(defcustom poetry-repository-list '("pypi")
  "List of repository name to register package to."
  :type '(repeat string))

(make-obsolete-variable 'poetry-repository-list
                        "Poetry.el now reads the repository list from Poetry settings. Please use `poetry config` to change the list of available repositories."
                        nil "0.2.0")


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

;;;###autoload (autoload 'poetry "poetry" nil t)
(define-transient-command poetry ()
  "Poetry menu."
  [:description (lambda ()
                  (let ((project-name (poetry-get-project-name)))
                    (if project-name
                        (format "Project: %s\n" project-name)
                      "Poetry\n")))
  [:if poetry-find-project-root
       :description "Dependencies    "
       ("a" "Add" poetry-add)
       ("r" "Remove" poetry-remove)
       ("i" "Install" poetry-install)
       ("l" "Lock" poetry-lock)
       ("u" "Update" poetry-update)
       ("s" "Show" poetry-show)]
  [:if poetry-find-project-root
    :description "Project"
        ("n" "New" poetry-new)
        ("e" "Edit 'pyproject.toml'" poetry-edit-pyproject-toml)
        ("c" "Check" poetry-check)
        ("b" "Build" poetry-build)
        ("p" "Publish" poetry-publish)]
  [:if-not poetry-find-project-root
    :description "Project"
    ("n" "New" poetry-new)
    ("I" "Init" poetry-init)]
  ]
  [[:if poetry-find-project-root
        :description "Shell"
        ("R" "Run a command" poetry-run)
        ("S" "Start a shell" poetry-shell)]
  [:if (lambda () (and (poetry-find-project-root)
                       (condition-case nil
                           (poetry-get-virtualenv)
                         (error nil))
                       (poetry-venv-activated-p)))
       :description "Virtualenv"
       ("v" "Deactivate" poetry-venv-deactivate)]
  [:if (lambda () (and (poetry-find-project-root)
                       (condition-case nil
                           (poetry-get-virtualenv)
                         (error nil))
                       (not (poetry-venv-activated-p))))
       :description "Virtualenv"
       ("v" "Activate" poetry-venv-workon)]
  ])
   ;; ["Poetry"
   ;;  ("U" "Update" poetry-self-update)]])

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
    (poetry-call 'add (cl-concatenate 'list
                                      (list package)
                                      args))))

;;;###autoload
(defun poetry-add-dep (package)
  "Add PACKAGE as a new dependency to the project.

PACKAGE can be a list of packages, separated by spaces."
  (interactive "sPackage name(s): ")
  (poetry-message (format "Adding dependency: %s" package))
  (poetry-call-add package '()))

;;;###autoload
(defun poetry-add-dev-dep (package)
  "Add PACKAGE as a new development dependency to the project.

PACKAGE can be a list of packages, separated by spaces."
  (interactive "sPackage name(s): ")
  (poetry-message (format "Adding dev dependency: %s" package))
  (poetry-call-add package '("-D")))

;;;###autoload
(defun poetry-add-opt-dep (package)
  "Add PACKAGE as a new optional dependency to the project.

PACKAGE can be a list of packages, separated by spaces."
  (interactive "sPackage name(s): ")
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
  (poetry-call 'remove (list package)))

(defun poetry-remove-dev-dep (package)
  "Remove PACKAGE from the project development dependencies."
  (poetry-call 'remove (list package "-D")))

;;;###autoload
(defun poetry-check ()
  "Check the validity of the pyproject.toml file."
  (interactive)
  (poetry-call 'check nil nil t t))

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
  (let ((compbufname (poetry-call 'show nil nil nil t)))
    (with-current-buffer compbufname
      (let (packs)
        (while (re-search-forward "^\\(.+\\)$" nil t)
          (push (match-string 1) packs))
        packs))))

;;;###autoload
(defun poetry-show (package)
  "Show information about package PACKAGE."
  (interactive
   (list (completing-read "Package: "
                          (poetry-show-get-packages))))
  (string-match "^\\([^[:space:]]*\\).*$" package)
  (poetry-call 'show (list (match-string 1 package)) nil t t))

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
                                 (or (poetry-publish-get-repositories)
                                     (poetry-error "No repository configured, please use `poetry config` to add repositories")
                                     )
                                 nil t)
                (read-string "Username: ")
                (read-passwd "Password: ")))
  (poetry-call 'publish
               (list "-r" repo "-u" username "-p" password)))

(defun poetry-publish-get-repositories ()
  "Return the list of configured repostitories."
  (let ((repos (poetry-get-configuration "repositories")))
    (mapcar #'car repos)))

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
    (poetry-call 'new (list path) path nil t)
    ;; Open __init__.py
    (find-file (concat (file-name-as-directory
                        (concat (file-name-as-directory path)
                                (poetry-normalize-project-name project-name)))
                       "__init__.py"))
    (save-buffer)
    ;; make sure the virtualenv is created
    (poetry-message "Creating the virtual environment...")
    (poetry-call 'run (split-string "python -V" "[[:space:]]+" t) nil nil t)
    (poetry-message "Done")
    ;; If tracking virtualenv, update the virtualenv
    (when poetry-tracking-mode
      (poetry-track-virtualenv))))

;;;###autoload
(defun poetry-init (&optional path)
  "Initialize a Poetry project in PATH."
  (interactive "GInitialize a project at: ")
  (let* ((path (expand-file-name (or path default-directory)))
         (default-directory path))
    (when (poetry-find-project-root)
      (poetry-error "'%s' is already a Poetry project" path))
    (shell (poetry-buffer-name "init"))
    (process-send-string (get-buffer-process
                          (get-buffer (poetry-buffer-name "init")))
                         "poetry init; exit\n")))

;;;###autoload
(defun poetry-edit-pyproject-toml ()
  "Open the current project 'pyproject.toml' file for edition."
  (interactive)
  (poetry-ensure-in-project)
  (find-file (poetry-find-pyproject-file)))

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
  (poetry-call 'run (split-string command "[[:space:]]+" t) nil t t))

;;;###autoload
(defun poetry-shell ()
  "Spawn a shell within the virtual environment."
  (interactive)
  (poetry-ensure-in-project)
  (shell (poetry-buffer-name "shell"))
  (process-send-string (get-buffer-process
                        (get-buffer (poetry-buffer-name "shell")))
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
  (when poetry-tracking-mode
    (poetry-error "Poetry tracking mode is activated, you should deactivate it before manually setting virtualenvs"))
  (poetry-ensure-in-project)
  (pyvenv-activate (poetry-get-virtualenv)))

;;;###autoload
(defun poetry-venv-deactivate ()
  "De-activate the virtualenv associated to the current poetry project."
  (interactive)
  (when poetry-tracking-mode
    (poetry-error "The current virtualenv has been set automatically by poetry tracking mode, deactivate the tracking mode to deactivate this virtualenv"))
  (let ((venv (poetry-get-virtualenv)))
    (if (not pyvenv-virtual-env)
        (poetry-error "No virtualenv activated")
      (if (not (poetry-venv-activated-p))
          (poetry-error "Current poetry virtualenv not activated")
        (pyvenv-deactivate)))))

;;;###autoload
(defun poetry-venv-toggle ()
  "Toggle the virtualenv associated to the current poetry project."
  (interactive)
  (if (poetry-venv-activated-p)
      (poetry-venv-deactivate)
    (poetry-venv-workon)))

(defun poetry-venv-exist-p ()
  "Return t if the current project has a venv."
  (poetry-get-virtualenv))

(defun poetry-venv-activated-p ()
  "Return t if the current project venv is activated."
  (let ((venv (poetry-get-virtualenv)))
    (and venv
         pyvenv-virtual-env
         (equal (file-name-as-directory (expand-file-name venv))
                (file-name-as-directory (expand-file-name
                                         pyvenv-virtual-env))))))


;; Virtualenv tracking
;;;;;;;;;;;;;;;;;;;;;;

(defvar poetry-venv-list '()
  "List of known poetry virtualenvs.")

(defvar poetry-saved-venv nil
  "Virtualenv activated before poetry.el changed it.

Allow to re-enable the previous virtualenv when leaving the poetry project.")


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
    (remove-hook 'post-command-hook 'poetry-track-virtualenv)

    ;; deactivate the current poetry virtualenv
    (when (and pyvenv-virtual-env
               (member (file-name-as-directory pyvenv-virtual-env)
                       poetry-venv-list))
      (if (not poetry-saved-venv)
          (pyvenv-deactivate)
        (pyvenv-activate poetry-saved-venv)
        (setq poetry-saved-venv nil)))))

(defun poetry-track-virtualenv ()
  "Automatically activate virtualenvs when visiting a poetry project."
  ;; Avoid massive slow down in Helm
  (when (not (string= (buffer-name) " *Minibuf-1*"))
  (cond
   ;; If in a poetry project, activate the associated virtualenv
   ((and buffer-file-name (poetry-find-project-root) (poetry-get-virtualenv))
    (let ((poetry-venv (poetry-get-virtualenv)))
      (when (and poetry-venv
                 (not (equal (file-name-as-directory poetry-venv)
                             pyvenv-virtual-env)))
        ;; Save previous virtualenv
        (when (and pyvenv-virtual-env
                   (not (member (file-name-as-directory pyvenv-virtual-env)
                                poetry-venv-list)))
          (setq poetry-saved-venv pyvenv-virtual-env))
        (add-to-list 'poetry-venv-list (expand-file-name
                                        (file-name-as-directory poetry-venv)))
        (pyvenv-activate poetry-venv))))
   ;; If not in a poetry project, deactivate the poetry virtualenv
   ((and pyvenv-virtual-env
         (member (file-name-as-directory pyvenv-virtual-env) poetry-venv-list))
    (if (not poetry-saved-venv)
        (pyvenv-deactivate)
      (pyvenv-activate poetry-saved-venv)
      (setq poetry-saved-venv nil))))))

;; Asynchroneous call to poetry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar poetry-call-queue '()
  "Poetry call queue.

Each element of the list is an operation to perform.
Operations are executed sequentially until the list is empty.")

(defvar poetry-process nil
  "Poetry current compilation process.")

(defun poetry-call (command &optional args project output blocking)
  "Call poetry COMMAND with the given ARGS.

PROJECT is the poetry project you want the command to be run for
\(default to the current project).
If OUTPUT is non-nil, display the compilation buffer.
If BLOCKING is non-nil, wait until the compilation is over and return the
compilation buffer name."
  ;; Wait for the queue to finish when making a blocking call
  (let (call-nmb (old-call-nmb -1))
    (while (and blocking (poetry--busy-p))
      (setq call-nmb (+ 1 (length poetry-call-queue)))
      (when (/= call-nmb old-call-nmb)
        (poetry-message "Waiting for %s operation%s to finish..."
                        call-nmb
                        (if (= call-nmb 1) "" "s")))
      (setq old-call-nmb call-nmb)
      (sleep-for .1)))
  ;; Add the call to the queue if already busy
  (if (poetry--busy-p)
      (add-to-list 'poetry-call-queue
                   (list command args (or project
                                          (poetry-find-project-root))
                         output blocking)
                   t)
    ;; Else, run the call
    (poetry-do-call command args project output blocking)))

(defun poetry-do-call (command &optional args project output blocking)
  "Call poetry COMMAND with the given ARGS.

Not queue-safe version of `poetry-call'.

PROJECT is the poetry project you want the command to be run for
\(default to the current project).
If OUTPUT is non-nil, display the compilation buffer.
If BLOCKING is non-nil, wait until the compilation is over and return the
compilation buffer name."
  (let ((default-directory (or project
                               (poetry-find-project-root)
                               default-directory)))
    (unless (member command '(new init config))
      (poetry-ensure-in-project))
    (let* ((prog (or (executable-find "poetry")
                     (poetry-error "Could not find 'poetry' executable")))
           (args (if (or (string= command "run")
                         (string= command "config")
                         (string= command "init"))
                     (cl-concatenate 'list (list (symbol-name command))
                                     args)
                   (cl-concatenate 'list (list
                                          (symbol-name command)
                                          "-n" "--ansi")
                                   args))))
      (let ((compilation-buffer-name-function
             (lambda (_mode) (poetry-buffer-name)))
            (compilation-ask-about-save nil)
            (compilation-save-buffers-predicate (lambda () nil)))
        (save-window-excursion
            (compile (concat prog " " (string-join args " "))))
        ;; compilation hooks
        (with-current-buffer (poetry-buffer-name)
          (add-hook 'after-change-functions
                    (lambda (beg end len)
                      (ansi-color-apply-on-region beg end))
                    nil t)
          (setq-local compilation-finish-functions
                      (append
                       compilation-finish-functions
                       (list
                        #'poetry--clean-compilation-buffer
                        #'poetry--indicate-compilation-end
                        #'poetry--run-next-call-from-queue))))
        (setq poetry-process
              (get-buffer-process (get-buffer (poetry-buffer-name))))
        ;; Block until completion if asked
        (when blocking
          (while (eq (process-status poetry-process) 'run)
            (sleep-for .1)))
        ;; Display the buffer if asked
        (if output
            (with-current-buffer (poetry-buffer-name)
              (let ((new-name (poetry-buffer-name "output")))
                (when (get-buffer new-name) (kill-buffer new-name))
                (poetry-display-buffer (rename-buffer new-name))
                new-name))
          (poetry-buffer-name))))))

(defun poetry--busy-p ()
  "Return t if the compilation process is busy."
  (let ((buff (get-buffer (poetry-buffer-name))))
    (when buff
      (let ((proc (get-buffer-process buff)))
        (when proc
         (eq (process-status proc) 'run))))))

(defun poetry--indicate-compilation-end (_compil-buf _msg)
  "Display a message in the minibuffer when the compilation is done."
  (message "Poetry finished"))

(defun poetry--clean-compilation-buffer (compil-buf _msg)
  "Clean the compilation buffer COMPIL-BUF from compilation messages."
  (when (string-match (poetry-buffer-name) (buffer-name compil-buf))
    (let ((beg (save-excursion (goto-char (point-min))
                               (forward-line 4)
                               (point)))
          (end (save-excursion (goto-char (point-max))
                               (forward-line -1)
                               (point))))
      (delete-region end (point-max))
      (delete-region (point-min) beg))))

(defun poetry--run-next-call-from-queue (compil-buf _msg)
  "Run the next call from the call queue (if there is one).

COMPIL-BUF is the current compilation buffer."
  (when (string-match (poetry-buffer-name) (buffer-name compil-buf))
    ;; Check if call went fine
    (unless (= (process-exit-status poetry-process) 0)
      (let ((new-name (poetry-buffer-name "error")))
        (when (get-buffer new-name) (kill-buffer new-name))
        (with-current-buffer (poetry-buffer-name)
          (rename-buffer new-name))
        ;; Save a copy in the default poetry buffer
        (with-current-buffer (get-buffer-create (poetry-buffer-name))
          (insert-buffer-substring new-name))
        (poetry-display-buffer new-name)
        (poetry-message "Error while running a poetry command."))))
  ;; Run the next queued call if necessary
  (when (/= (length poetry-call-queue) 0)
    (let ((call-args (car poetry-call-queue)))
      (setq poetry-call-queue (cdr poetry-call-queue))
      (apply #'poetry-do-call call-args))))



;; Helpers
;;;;;;;;;;

(defun poetry-get-configuration (key)
  "Return Poetry configuration for KEY.

\(type `poetry config --list' to get a list of usable configuration keys.)"
  (let ((bufname (poetry-call 'config (list key) nil nil t)))
    (with-current-buffer bufname
      (when (progn
              (goto-char (point-min))
              (re-search-forward "\\[ValueError\\]" nil t))
        (poetry-error "Unrecognized key configuration: %s" key))
      (goto-char (point-min))
      ;; Parse as JSON if possible, otherwise return trimmed string
      (let* ((json-key-type 'string)
	     (json-false nil)
             (data (buffer-substring-no-properties
                    (point-min) (point-max)))
             (rawconfig (replace-regexp-in-string
                         "'" "\"" data)))
	(condition-case nil
	    (json-read-from-string rawconfig)
	  (error (string-trim rawconfig)))))))

(defun poetry-buffer-name (&optional suffix)
  "Return the poetry buffer name, using SUFFIX is specified."
  (if suffix
      (format "*poetry-%s*" suffix)
    "*poetry*"))

(defun poetry-normalize-project-name (project-name)
  "Return a normalized version of the PROJECT-NAME."
  (replace-regexp-in-string "-+" "_" (downcase project-name)))

(defun poetry-display-buffer (&optional buffer-name)
  "Display the poetry buffer or the BUFFER-NAME buffer."
  (with-current-buffer (or buffer-name (poetry-buffer-name))
    (let ((buffer-read-only nil))
      (display-buffer (or buffer-name (poetry-buffer-name))))))

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
           (regex
            "^\\(?1:[^= ]*\\)[[:space:]]*=[[:space:]]*\\({\\|\"\\)\\(?2:.*\\)\\(}\\|\"\\)")
           deps
           filtered-deps)
       (goto-char beg)
       (while (re-search-forward regex end t)
         (push (format "%s (%s)"
                       (substring-no-properties (match-string 1))
                       (substring-no-properties (match-string 2)))
               deps))
       ;; clean from opt/not opt deps
       (dolist (dep deps)
         (if opt
             (when (string-match "optional = true" dep)
               (push (replace-regexp-in-string ",?[[:space:]]*optional = true" "" dep)
                     filtered-deps))
           (when (not (string-match "optional = true" dep))
               (push dep filtered-deps))))
       filtered-deps)))

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
  "Return the current poetry project virtualenv, or nil if it does not exist."
  (poetry-ensure-in-project)
  (if (and poetry-project-venv
           (file-exists-p poetry-project-venv))
      poetry-project-venv
    (setq poetry-project-venv
          (or
           ;; virtualenvs in project
           (if (poetry-get-configuration "virtualenvs.in-project")
               (concat (file-name-as-directory (poetry-find-project-root))
                       ".venv")
             ;; virtualenvs elsewhere
             (car (directory-files
                   (poetry-get-configuration "virtualenvs.path")
                   t
                   (format "%s-py"
                           (poetry-get-project-name)))))
           nil))))

(defun poetry-find-pyproject-file ()
  "Return the location of the 'pyproject.toml' file."
  (let ((root (poetry-find-project-root)))
    (when root
      (concat (file-name-as-directory root) "pyproject.toml"))))

(defun poetry-ensure-in-project ()
  "Return an error if not in a poetry project."
  (unless (poetry-find-project-root)
    (poetry-error "Not in a poetry project")))

(defun poetry-message (format-string &rest args)
  "Display the message FORMAT-STRING formatted with ARGS."
  (message "[%s] %s" (or (poetry-get-project-name) "Poetry")
           (apply #'format-message format-string args)))

(defun poetry-error (format-string &rest args)
  "Display the error FORMAT-STRING formatted with ARGS."
  (error "[%s] %s" (or (poetry-get-project-name) "Poetry")
         (apply #'format-message format-string args)))


(provide 'poetry)
;;; poetry.el ends here
