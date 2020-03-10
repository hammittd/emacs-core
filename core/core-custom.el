;; custom.el -- Custom module
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

(defcustom my-theme 'zenburn
  "Default color scheme."
  :type 'symbol
  :group 'core)

(provide 'core-custom)
