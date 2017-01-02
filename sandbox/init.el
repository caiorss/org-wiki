;; (setq user-emacs-directory "sandbox")
;; (setq pacakge-user-dir "sandbox/elpa")

(setq user-emacs-directory (getenv "USER_DIRECTORY"))
(setq pacakge-user-dir     (getenv "PACKAGE_DIR"))
(setq user-init-file       (getenv "INITFILE"))

;; (setq default-directory    (getenv "DEFAULT_DIRECTORY"))

;;; Separate customization form init file 
(setq custom-file (concat (file-name-as-directory user-emacs-directory) "custom.el"))
(load custom-file 'noerror)


(setq package-archives
      '(
	
	;;("melpa" . "https://melpa.milkbox.net/packages/")
	;;("popkit" . "http://elpa.popkit.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	
	;; ("org"       . "http://orgmode.org/elpa/")
	("gnu"       . "http://elpa.gnu.org/packages/")

	;; ("marmalade" .  "http://marmalade-repo.org/packages/")

    ))

(package-initialize)

(defun packages-require (&rest packs)
  "Install and load a package. If the package is not available
installs it automaticaly."
  (mapc  (lambda (package)
           (unless (package-installed-p package)
                   (package-install package)    
                   ;;#'package-require
                   ))
         packs        
         ))

(unless (file-exists-p "elpa")
  (package-refresh-contents))

(unless (package-installed-p 'org-wiki)
  (package-install-file "../org-wiki.el"))

(setq org-wiki-location (getenv "ORG_WIKI_LOCATION"))


(unless (file-exists-p org-wiki-location)
  (mkdir org-wiki-location))

(packages-require  'htmlize)


;; Print environment variables in the *scratch* buffer.
;;
(defun test-sandbox ()
  "Test if the sandbox is working."
  (interactive)
  (switch-to-buffer "*scratch*")
  (insert (concat "user-emacs-directory = "  user-emacs-directory "\n" ))
  (insert (concat "user-init-file = "        user-init-file "\n" ))
  (insert (concat "package-user-dir = "      package-user-dir "\n" )))

;;; Print Sandbox environment variables to check if it works.
(test-sandbox)


;;; Necessary for colored source code blocks
(require 'htmlize)

(require 'org-wiki)

(org-wiki-index)
(org-toggle-inline-images)
