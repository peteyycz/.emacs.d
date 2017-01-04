(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'cfg-package)

(peteyy/ensure-package-installed
 'magit
 'projectile
 'smex
 'ido-vertical-mode
 'whole-line-or-region)

;; Ido vertical mode works better for me
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Disable toolbar
(tool-bar-mode -1)

;; Cut and copy whole line or region mode
(whole-line-or-region-mode 1)

;; Projectile mode
(projectile-mode 1)

;; Configure backup file creation in it's own directory
(defvar peteyy/backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p peteyy/backup-directory)
  (make-directory peteyy/backup-directory t))
(setq backup-directory-alist `(("." . ,peteyy/backup-directory))
      auto-save-file-name-transforms
          `(("." ,peteyy/backup-directory t))
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
