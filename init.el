(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Remove cluttered toolbar
(tool-bar-mode -1)

;; Nice built-in completion-system
(ido-mode 1)

;; Disable autosave because it's slow
(auto-save-mode -1)

;; Git integration
(use-package magit
  :ensure t
  :bind ("C-x C-g" . magit-status))

;; Manage projects with a keystroke
(use-package projectile
  :ensure t
  :config (projectile-mode 1))

;; Ido flavored M-x
(use-package smex
  :ensure t
  :bind (("M-x" . smex)))

;; Vertical alignment for ido
(use-package ido-vertical-mode
  :ensure t
  :init (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config (ido-vertical-mode 1))

;; If nothing is marked yanks whole line
(use-package whole-line-or-region
  :ensure t
  :config (whole-line-or-region-mode 1))

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
	mac-option-modifier 'meta))

(when (eq window-system 'mac)
  (peteyy/mac-mode-hook))
