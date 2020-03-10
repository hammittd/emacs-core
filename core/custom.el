;; custom.el -- Custom module
;;
;; Derek Hammitt
;; 2020-03-10

(defgroup core nil
  "Emacs core group configuration."
  :prefix "core-"
  :group 'convenience)

(defcustom my-theme 'zenburn
  "Default color scheme."
  :type 'symbol
  :group 'core)

(provide 'core-custom)
