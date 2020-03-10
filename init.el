;; init.el -- initiation file
;; 
;; Derek's Emacs init file
;; 2020-03-10
;;
;; Description:
;; This is not part of Emacs. These are personal emacs
;; settings and configurations.

(defvar root-dir (file-name-directory load-file-name)
  "Root emacs directory.")
(defvar core-dir (expand-file-name "core" root-dir)
  "Core modules directory.")
(defvar custom-dir (expand-file-name "custom" root-dir)
  "Directory for Customize UI settings file.")
(defvar modules-dir (expand-file-name "modules" root-dir)
  "Secondary modules directory.")
(defvar core-savefiles-dir (expand-file-name "autosaves" root-dir)
  "Directory for all save files and history files.")
(unless (file-exists-p core-savefiles-dir)
  (make-directory core-savefiles-dir))

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)

;; Reduce Garbage Collection to 25MB
(setq gc-cons-threshold 25000000)
;; Large File warnings > 100MB
(setq large-file-warning-threshold 100000000)

;; Load core modules
(require 'core-packages)
(require 'core-custom)
(require 'core-editor)
;; Core Editor settings and keybindings
(all-settings)
(use-all-keybindings)
(backup-to-tmp)
;; User Interface Settings
(require 'core-ui)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (zenburn-theme projectile smartparens ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
