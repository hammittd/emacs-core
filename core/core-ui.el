;; core-ui.el -- User Interface configuration
;;
;; Derek Hammitt
;; 2020-03-10
;;
;; Description:
;; Configure for Emacs' user interface. The theme and other
;; core-group variables can be edited in core-custom.el
;;
;; Code:

;; Frame Settings

(defun remove-menu-bar ()
  (menu-bar-mode -1))

(defun remove-tool-bar ()
  (tool-bar-mode -1))

;; Scrolling
(defun set-scroll-config ()
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)
  (scroll-bar-mode -1))

;; Line Numbers
(defun set-line-numbers ()
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t))

;; Theme
(defun set-theme ()
  (when core-theme
    (load-theme core-theme t)))

;; Enable Settings
(defun use-all-settings ()
  (remove-menu-bar)
  (remove-tool-bar)
  (set-scroll-config)
  (set-line-numbers)
  (set-theme))

(use-all-settings)

(provide 'core-ui)

;; core-ui.el ends here
