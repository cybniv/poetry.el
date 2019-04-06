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

(require 'transient)
(require 'toml)


;; Transient interface
(define-transient-command poetry ()
  "Poetry menu."
  :man-page "poetry"
  [:if poetry-find-project-root
   :description "Dependencies"
    ("a" "Add" poetry-add)
    ("r" "Remove" poetry-remove)
    ("i" "Install" poetry-install)
    ("l" "Lock" poetry-lock)
    ("u" "Update" poetry-update)
    ("s" "Show" poetry-show)]
  [:if poetry-find-project-root
   :description "Packages"
   ("b" "Build" poetry-build)
   ("p" "Publish" poetry-publish)]
  ["Project"
   ("I" "Init" poetry-init)
   ("n" "New" poetry-new)
   ("c" "Check" poetry-check)]
  [:if poetry-find-project-root
   :description "Shell"
   ("R" "Run" poetry-run)
   ("S" "Start a shell" poetry-shell)]
  [:if poetry-find-project-root
   :description "Cache"
   ("C" "Clear" poetry-clear)]
  ["Poetry"
   ("u" "Update" poetry-update)])

;; Poetry commands
(defun poetry-add (package)
  "Add PACKAGE as a new dependency to the project."
  (interactive "sPackage name: ")
  (poetry-call 'add nil package))

(defun poetry-remove (package)
  "Removes PACKAGE from the project dependencies."
  (interactive (list (car (split-string
                           (completing-read "Package: "
                                            (poetry-get-dependencies)
                                            nil 'confirm)
                           "[[:space:]]+"))))
  (poetry-call 'remove nil package))

(defun poetry-check ()
  "Checks the validity of the pyproject.toml file."
  (interactive)
  (poetry-call 'check))

(defun poetry-install ()
  "Installs the project dependencies."
  (interactive)
  (poetry-call 'install))

(defun poetry-lock ()
  "Locks the project dependencies."
  (interactive)
  (poetry-call 'lock))

(defun poetry-update ()
  "Update dependencies as according to the pyproject.toml file."
  (interactive)
  (poetry-call 'update))

(defun poetry-show ()
  "Shows information about packages."
  (interactive)
  (poetry-call 'show))

(defun poetry-build ()
  "Builds a package, as a tarball and a wheel by default."
  (interactive)
  (poetry-call 'build))

(defun poetry-publish ()
  "Publishes a package to a remote repository."
  (interactive)
  (poetry-call 'publish))

(defun poetry-init (path)
  "Creates a basic pyproject.toml file at PATH."
  (interactive "DProject path: ")
  (poetry-call 'init nil path))

(defun poetry-new (path)
  "Creates a new Python project at PATH"
  (interactive "DProject path: ")
  (poetry-call 'new nil path))

(defun poetry-check ()
  "Checks the validity of the pyproject.toml file."
  (interactive)
  (poetry-call 'check t))

(defun poetry-run (command)
  "Runs a command in the appropriate environment."
  (interactive "sCommand: ")
  (poetry-call 'run nil command))

(defun poetry-shell ()
  "Spawns a shell within the virtual environment."
  (interactive)
  (poetry-call 'shell))

(defun poetry-clear ()
  "Clears poetry's cache."
  (interactive)
  (poetry-call 'clear))

(defun poetry-update ()
  "Update dependencies as according to the pyproject.toml file."
  (interactive)
  (poetry-call 'update))

;; Helpers
(defun poetry-call (command &optional output &rest args)
  "Call poetry COMMAND with the given ARGS"
  (apply 'start-process (concatenate 'list (list "poetry" " *poetry*"
                                             "poetry" (symbol-name command))
                                     args))
  (when output
    (view-buffer-other-window " *poetry*")))

(defun poetry-get-dependencies ()
  "Return the list of project dependencies."
  (interactive)
  (let* ((pyproject-file (poetry-find-pyproject-file))
         (pyproject (toml:read-from-file pyproject-file))
         (dependencies (cdr (assoc "dependencies"
                                   (assoc "poetry"
                                          (assoc "tool" pyproject))))))
    (map 'list (lambda (elem) (format "%s (%s)" (car elem) (cdr elem)))
         dependencies)))

(defun poetry-find-project-root ()
  "Return the poetry project root if any."
  (locate-dominating-file default-directory "pyproject.toml"))

(defun poetry-find-pyproject-file ()
  "Return the location of the 'pyproject.toml' file."
  (let ((root (poetry-find-project-root)))
    (when root
      (concat (file-name-as-directory root) "pyproject.toml"))))

;; toml fixes
;; support hyphens (-) in group/var names
(defun toml:read-keygroup ()
  (toml:seek-readable-point)
  (let (keygroup)
    (while (and (not (toml:end-of-buffer-p))
                (char-equal (toml:get-char-at-point) ?\[))
      (if (toml:search-forward "\\[\\([a-zA-Z][a-zA-Z0-9_\\.-]*\\)\\]")
          (let ((keygroup-string (match-string-no-properties 1)))
            (when (string-match "\\(_\\|\\.\\)\\'" keygroup-string)
              (signal 'toml-keygroup-error (list (point))))
            (setq keygroup (split-string (match-string-no-properties 1) "\\.")))
        (signal 'toml-keygroup-error (list (point))))
      (toml:seek-readable-point))
    keygroup))

(defun toml:read-key ()
  (toml:seek-readable-point)
  (if (toml:end-of-buffer-p) nil
    (if (toml:search-forward "\\([a-zA-Z][a-zA-Z0-9_-]*\\) *= *")
        (let ((key (match-string-no-properties 1)))
          (when (string-match "_\\'" key)
            (signal 'toml-key-error (list (point))))
          key)
      (signal 'toml-key-error (list (point))))))

(provide 'poetry)
;;; poetry.el ends here
