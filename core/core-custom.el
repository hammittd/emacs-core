;; core-custom.el -- Custom module
;;
;; Derek Hammitt
;; 2020-03-10
;;
;; Description:
;; This creates a `core' group that can be used
;; for further customization within different modules.

(defgroup core nil
  "Emacs core group configuration."
  :prefix "core-"
  :group 'convenience)

(defcustom core-theme 'zenburn
  "Default color scheme."
  :type 'symbol
  :group 'core)

(defcustom core-whitespace t
  "whitespace visibility."
  :type 'boolean
  :group 'core)

(defcustom core-clean-whitespace-on-save t
  "Clean whitespace from file before save.
  core-whitespace must also be enabled."
  :type 'boolean
  :group 'core)

(provide 'core-custom)

;; core-custom.el ends here
