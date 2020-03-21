;; core-editor.el -- Editor Configuration and Defaults
;;
;; Derek Hammitt
;; 2020-03-10
;;
;; Description:
;; Utilities, settings, and keybindings for the editor.
;; Also includes two fuctions that will use all Settings and Keybindings
;;
;; Code:

;; Utilities

(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments a region or the current line if there's no region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun disable-tabs () (setq indent-tabs-mode nil))

(defun enable-tabs ()
  (let ((custom-tab-width 2))
    (local-set-key (kbd "TAB") 'tab-to-tab-stop)
    (setq indent-tabs-mode t)
    (setq tab-width custom-tab-width)))

(defun reset-text-size ()
  (interactive)
  (text-scale-set 0))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))

;; Settings
(defun activate-ivy-mode ()
  (ivy-mode 1))

(defun always-highlight-code ()
  "Syntax highlight where possible."
  (global-font-lock-mode t))

(defun append-newline ()
  (setq require-final-newline t))

(defun apply-changes-to-highlighted-region ()
  "Transient mark mode."
  (transient-mark-mode t))

(defun auto-save-on ()
  (add-hook 'text-mode-hook #'auto-save-visited-mode)
  (setq auto-save-timeout 3)
  (add-hook 'auto-save-hook #'save-all-buffers))

(defun flash-instead-of-bell ()
  (setq visible-bell t))

(defun offer-to-create-dirs-on-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))

(defun overwrite-selection ()
  (delete-selection-mode t))

(defun prog-mode-tab-hook ()
  (add-hook 'prog-mode-hook 'enable-tabs))

(defun quiet-startup ()
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun refresh-buffers-when-files-change ()
  (global-auto-revert-mode t))

(defun save-scripts-as-executable ()
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(defun shorten-yes-or-no ()
  (fset 'yes-or-no-p 'y-or-n-p))

(defun show-matching-parens ()
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

(defun set-default-line-length-to (line-length)
  (setq-default fill-column line-length))

(defun smartparens-config ()
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1))

;;(defun spaces-not-tabs ()
;;   (setq-default indent-tabs-mode nil)
;;   (setq-default tab-width 8))

;;(defun tab-autocomplete ()
;;  (setq tab-always-indent 'complete))

(defun treat-camelcase-as-separate-words ()
  (add-hook 'prog-mode-hook 'subword-mode))

(defun core-cleanup-whitespace ()
  "Invoke `whitespace-cleanup' if `core-clean-whitespace-on-save is not nil."
  (when core-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun core-enable-whitespace ()
  "Enable whitespace-mode if `core-whitespace' is not nil. see core-custom.el"
  (when core-whitespace
    ;; clean up whitespace!
    (add-hook 'before-save-hook 'core-cleanup-whitespace nil t)
    (global-whitespace-mode t)
    (setq whitespace-global-modes '(not org-mode))))

(defun whitespace-settings ()
  (setq whitespace-style '(face tabs tab-mark empty trailing lines-tail))
  (setq whitespace-display-mappings '(
    (tab-mark     ?\t    [?\u007C ?\t] [?\\ ?\t])
  ))
  (core-enable-whitespace))

;; Use all the settings:
(defun all-settings ()
  (activate-ivy-mode)
  (auto-save-on)
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
  (prog-mode-tab-hook)
  (set-default-line-length-to 80)
  (smartparens-config)
  (whitespace-settings))

;; Keybindings

(defun bind-beginning-of-line-or-indentation ()
  (global-set-key (kbd "C-a") 'beginning-of-line-or-indentation))

(defun bind-comment-uncomment-to-meta-semi ()
  (global-set-key (kbd "M-;")
                  'comment-or-uncomment-region-or-line))

(defun bind-ibuffer-instead-of-buffer-menu ()
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(defun bind-keys-to-change-text-size ()
  "Bind C-+ and C-- to increase and decrease text size,
respectively."
  (define-key global-map (kbd "C-)") 'sensible-defaults/reset-text-size)
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C-=") 'text-scale-increase)
  (define-key global-map (kbd "C-_") 'text-scale-decrease)
  (define-key global-map (kbd "C--") 'text-scale-decrease))

(defun use-ivy-isearch ()
  (global-set-key (kbd "C-s") 'swiper-isearch))

(defun use-all-keybindings ()
  (bind-beginning-of-line-or-indentation)
  (bind-comment-uncomment-to-meta-semi)
  (bind-ibuffer-instead-of-buffer-menu)
  (bind-keys-to-change-text-size)
  (use-ivy-isearch))

;; Backup to /tmp directory
(defun backup-to-tmp ()
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(provide 'core-editor)

;; core-editor.el ends here
