;; core-rust.el -- Rust mode configuration
;;
;; Derek Hammitt
;;
;; Description:
;;
;; Code:

(require 'core-programming)

(core-require-packages '(rust-mode
                         cargo))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'core-rust)

;; core-rust.el ends here
