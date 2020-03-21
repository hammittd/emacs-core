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
(defvar personal-dir (expand-file-name "personal" root-dir)
  "Director for storing modules with personal settings. e.g.,
  choosing which modes to load via `core-modules-to-load'.")
(defvar core-modules-to-load-file (expand-file-name "core-modules.el" personal-dir)
  "Add desired modules to load to this file.")
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

;; Set editor settings and keybindings from `core-editor'
(all-settings)
(use-all-keybindings)
(backup-to-tmp)

;; User Interface Settings
(require 'core-ui)

;; Add modules from `modules-dir', to core-modules.el in `personal-dir'
;; to load modules here:
(if (file-exists-p core-modules-to-load-file)
    (progn
      (load core-modules-to-load-file)))

;; Set custom-file
(setq custom-file (expand-file-name "emacs-custom.el" custom-dir))
(if (file-exists-p custom-file)
    (load custom-file))
