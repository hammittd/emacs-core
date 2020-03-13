;; core-programming.el -- Core programming mode, additional configuration of emacs prog-mode
;;
;; Derek Hammitt
;;
;; Description:
;; Some additional configuration for programming-related utilities
;;
;; Code:

(defun core-programming-mode-defaults ()
  "Default settings hook."
  (smartparens-mode +1)
  (core-enable-whitespace))

(setq core-prog-mode-hook 'core-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'core-prog-mode-hook)))

(provide 'core-programming)

;; core-programming.el ends here
