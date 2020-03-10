;; core-editor.el -- Editor Configuration and Defaults
;;
;; Derek Hammitt
;; 2020-03-10

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments a region or the current line if there's no region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg-end)))

(defun reset-text-size ()
  (interactive)
  (text-scale-set 0))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))

;; Settings
(defun auto-save-on ()
  (add-hook 'text-mode-hook #'auto-save-visited-mode)
  (setq auto-save-timeout 1)
  (add-hook 'auto-save-hook #'save-all-buffers))

(defun delete-trailing-whitespace ()
  "Call DELETE-TRAILING-WHITESPACE when a buffer is saved."
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun treat-camelcase-as-separate-words ()
  (add-hook 'prog-mode-hook 'subword-mode))

(defun save-scripts-as-executable ()
  (add-hook 'after-save-hook
	    'executable-make-buffer-file-executable-if-script-p))

(defun offer-to-create-dirs-on-save ()
  (add-hook 'before-save-hook
	    (lambda ()
	      (when buffer-file-name
		(let ((dir (file-name-directory buffer-file-name)))
		  (when (and (not (file-exists-p dir))
			     (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
		    (make-directory dir t)))))))
;;
(defun apply-changes-to-highlighted-region ()
  "Transient mark mode."
  (transient-mark-mode t))

(defun overwrite-selection ()
  (delete-selection-mode t))

(defun append-newline ()
  (setq require-final-newline t))

(defun quiet-startup ()
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun shorten-yes-or-no ()
  (fset 'yes-or-no-p 'y-or-n-p))

(defun always-highlight-code ()
  "Syntax highlight where possible."
  (global-font-lock-mode t))

(defun refresh-buffers-when-files-change ()
  (global-auto-revert-mode t))

(defun show-matching-parens ()
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

(defun flash-instead-of-bell ()
  (setq visible-bell t))

(defun set-default-line-length-to (line-length)
  (setq-default fill-column line-length))

(defun spaces-not-tabs ()
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 8))

(defun tab-autocomplete ()
  (setq tab-always-indent 'complete))

(defun smartparens-config ()
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1))

(defun all-settings ()
  (auto-save-on)
  (delete-trailing-whitespace)
  (treat-camelcase-as-separate-words)
  (save-scripts-as-executable)
  (offer-to-create-dirs-on-save)
  (apply-changes-to-highlighted-region)
  (overwrite-selection)
  (append-newline)
  (quiet-startup)
  (shorten-yes-or-no)
  (always-highlight-code)
  (refresh-buffers-when-files-change)
  (show-matching-parens)
  (flash-instead-of-bell)
  (set-default-line-length-to 80)
  (spaces-not-tabs)
  (tab-autocomplete)
  (smartparens-config))

;; Keybindings
(defun bind-comment-uncomment-to-meta-semi ()
  (global-set-key (kbd "M-;")
		  'comment-or-uncomment-region-or-line))

(defun bind-keys-to-change-text-size ()
  "Bind C-+ and C-- to increase and decrease text size,
respectively."
  (define-key global-map (kbd "C-)") 'sensible-defaults/reset-text-size)
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C-=") 'text-scale-increase)
  (define-key global-map (kbd "C-_") 'text-scale-decrease)
  (define-key global-map (kbd "C--") 'text-scale-decrease))

(defun use-all-keybindings ()
  (bind-comment-uncomment-to-meta-semi)
  (bind-keys-to-change-text-size))

;; Backup to /tmp directory
(defun backup-to-tmp ()
  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t))))

(provide 'core-editor)

;; core-editor.el ends here
