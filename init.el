(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'cfg-package)

(peteyy/ensure-package-installed
 'magit
 'projectile
 'smex
 'ido-vertical-mode)

;; Ido vertical mode works better for me
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Disable toolbar
(tool-bar-mode -1)

;; Configure backup file creation in it's own directory
(defvar peteyy/backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p peteyy/backup-directory)
  (make-directory peteyy/backup-directory t))
(setq backup-directory-alist `(("." . ,peteyy/backup-directory))
      make-backup-files t
      version-control t
      backup-by-copying-when-linked t
      delete-old-versions t
      delete-by-moving-to-trash t
      auto-save-default t)

;; Custom keybindings
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "M-x") 'smex)

;; Change customize-* file
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun peteyy/mac-mode-hook ()
  (setq mac-command-modifier 'meta
	mac-option-modifier 'meta))

(when (eq window-system 'mac)
  (peteyy/mac-mode-hook))
