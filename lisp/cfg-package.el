;; Original source http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  ;; Make sure to have downloaded archive description.
  (package-refresh-contents)
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

(defun peteyy/ensure-package-installed (&rest packages)
  (apply 'ensure-package-installed packages)
  ;; activate installed packages
  (package-initialize))

(provide 'cfg-package)
