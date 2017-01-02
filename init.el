(when (eq window-system 'mac)
  (peteyy-mac-mode-hook))

(defun peteyy-mac-mode-hook ()
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'magit) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)
