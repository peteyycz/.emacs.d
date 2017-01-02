(require 'cfg-package)

(peteyy-ensure-package-installed 'magit)

(global-set-key (kbd "C-x C-g") 'magit-status)

(when (eq window-system 'mac)
  (peteyy-mac-mode-hook))

(defun peteyy-mac-mode-hook ()
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))



