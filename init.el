(package-initialize)

(require 'use-package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Remove cluttered toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(hl-line-mode 1)
(setq-default visible-bell 0
              indent-tabs-mode nil)

;; Keeps me from fat fingering C-x b
(global-set-key (kbd "C-x C-b") 'counsel-switch-to-buffer-or-window)

;; Disable autosave because it's slow
(auto-save-mode -1)

;; Parentheses (&friends) helper
(electric-pair-mode 1)

;; Save adding :ensure t on every use package
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package all-the-icons)
(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Git integration
(use-package magit
  :bind ("C-x C-g" . magit-status))
(use-package github-browse-file)
(use-package gist)

(use-package counsel)
(use-package counsel-projectile
  :config (counsel-projectile-mode))
(use-package ivy
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'swiper)
  ("C-c C-r" . 'ivy-resume)
  ("C-x C-f" . 'counsel-find-file)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  )

;; Searching with projectile
(use-package rg)
(use-package projectile-ripgrep)
;; Manage projects with a keystroke
(use-package projectile
  :config (projectile-mode 1))

;; If nothing is marked yanks whole line
(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode 1))

;; Autocomplete Popups
(use-package company
  :config (global-company-mode 1))

;; Gitgutter
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode 1))

;; Yaml editing support
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package org-bullets
  :hook org-mode)

;; Web mode
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :config (setq
	   web-mode-markup-indent-offset 2
	   web-mode-code-indent-offset 2))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :config (setq typescript-indent-level 2))

(require 'cl) ;; Required for vala mode for set-difference function
(use-package vala-mode
  :mode ("\\.vala\\'" . vala-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; JavaScript mode
;; Better highlighting for JS files (potential support for JSX too)
(use-package js2-mode
  :interpreter ("node" . js2-mode)
  :mode ("\\.m?jsx?\\'" . js2-mode)
  :config (setq js2-basic-offset 2
                js2-indent-switch-body t
		js2-strict-missing-semi-warning nil
                js2-mode-show-strict-warnings nil))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/tslint/bin/tslint"
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

(use-package flycheck
  :hook ((flycheck-mode . my/use-tslint-from-node-modules)
         (flycheck-mode . my/use-eslint-from-node-modules))
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode))

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
  :config (load-theme 'solarized-light))

(use-package yasnippet
  :config (setq yas-snippet-dirs
                '("~/.emacs.d/snippets")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(setq-default cursor-type 'box)
(set-frame-font "Roboto Mono 11")

;; Disable backup files (# and ~ files)
(setq make-backup-files nil
      auto-save-default nil)

;; Change customize-* file
(setq custom-file (concat user-emacs-directory "custom.el"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
