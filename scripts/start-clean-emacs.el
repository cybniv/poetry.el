;; Add melpa
(require 'package)
(setq package-archives '(("MELPA"        . "https://melpa.org/packages/")))
(package-initialize)
;; Install dependencies
(package-install 'transient)
(package-install 'pyvenv)
;; Load local poetry.el
(require 'f)
(require 'cl-extra)
(let ((poetry-dir (f-parent (f-dirname (f-this-file)))))
  (setq load-path (cons poetry-dir load-path)))
(require 'poetry)
