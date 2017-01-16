(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Remove cluttered toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1)

;; Nice built-in completion-system
(ido-mode 1)

;; Disable autosave because it's slow
(auto-save-mode -1)

;; Parentheses (&friends) helper
(electric-pair-mode 1)

;; Save adding :ensure t on every use package
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package all-the-icons)
(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Git integration
(use-package magit
  :bind ("C-x C-g" . magit-status))

(use-package ag)
;; Manage projects with a keystroke
(use-package projectile
  :config (projectile-mode 1))

;; Ido flavored M-x
(use-package smex
  :bind (("M-x" . smex)))

;; Vertical alignment for ido
(use-package ido-vertical-mode
  :init (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config (ido-vertical-mode 1))

;; If nothing is marked yanks whole line
(use-package whole-line-or-region
  :config (whole-line-or-region-mode 1))

;; Autocomplete Popups
(use-package company
  :config (global-company-mode 1))

;; Yaml editing support
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Web mode
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :config (setq
	   web-mode-markup-indent-offset 2
	   web-mode-code-indent-offset 2))

;; JavaScript mode
;; Better highlighting for JS files (potential support for JSX too)
(use-package js2-mode
  :interpreter ("node" . js2-mode)
  :mode ("\\.jsx?\\'" . js2-mode)
  :config (setq js2-basic-offset 2
		js2-strict-missing-semi-warning nil))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

;; Maybe I can finally start using it (maybe)
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; 4daLookz
(use-package solarized-theme
  :config (load-theme 'solarized-dark)
  :init (setq solarized-use-less-bold t
	      solarized-use-more-italic t))
(setq-default cursor-type 'bar)
(set-frame-font
 (concat "Input Mono Condensed "
	 (number-to-string (if (eq (window-system) 'x)
			       13 ;; Linux
			     14)))) ;; Mac

;; Configure backup file creation in it's own directory
(defvar peteyy/backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p peteyy/backup-directory)
  (make-directory peteyy/backup-directory t))
(setq backup-directory-alist `(("." . ,peteyy/backup-directory))
      make-backup-files t
      version-control t
      backup-by-copying-when-linked t
      delete-old-versions t
      delete-by-moving-to-trash t)

;; Change customize-* file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Smart C-a
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

(defun peteyy/mac-mode-hook ()
  (setq mac-command-modifier 'meta
	mac-option-modifier 'meta)
  (mac-auto-operator-composition-mode))

(when (eq window-system 'mac)
  (peteyy/mac-mode-hook))
