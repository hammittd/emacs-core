;; core-packages.el -- Packages do download and install, if not already present
;;
;; Derek Hammitt
;; 2020-03-10

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" root-dir))
(package-initialize)

(defvar core-packages
  '(ace-window
    counsel
    highlight-indent-guides
    ivy
    smartparens
    projectile
    swiper
    zenburn-theme)
  "Packages to be installed at launch.")

(defun core-packages-installed-p ()
  "Check which packages are installed from `core-packages'."
  (every #'package-installed-p core-packages))

(defun core-require-package (package)
  "Install PACKAGE unless it's installed."
  (unless (memq package core-packages)
    (add-to-list 'core-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun core-require-packages (packages)
  "Ensure PACKAGES are installed. Missing packages are installed."
  (mapc #'core-require-package packages))

(defun core-install-packages ()
  "Install all packages listed in `core-packages'."
  (unless (core-packages-installed-p)
    ;; Check for new packages
    (message "%s" "Emacs is refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install missing packages
    (core-require-packages core-packages)))

(core-install-packages)

;; Packages for various modes:
(defmacro core-auto-install (extension package mode)
  "When a file with a certain EXTENSION is opened, install the PACKAGE
  and open the file in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar core-auto-install-alist
  '(("\\.rs\\'" rust-mode rust-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)))

(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (core-auto-install extension package mode))))
 core-auto-install-alist)

(provide 'core-packages)

;; core-packages.el ends here
