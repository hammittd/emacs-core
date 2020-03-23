;;; config.el --- $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Personal Emacs config, extending Emacs doom:
;; https://github.com/hlissner/doom-emacs
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;; Code:
(setq user-full-name "Derek Hammitt"
      user-mail-address "derekrhammitt@gmail.com"

      doom-scratch-buffer-major-mode 'org-mode
      doom-theme 'doom-nord
      treemacs-width 32

      ;; Don't display line numbers
      display-line-numbers-type nil

      ;; lsp-ui-sideline redundant with eldoc
      lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil

      ;; Disable help mouse-overs for mode-line segments
      mode-line-default-help-echo nil
      show-help-function nil)

;;; UI
(setq doom-font (font-spec :family "monospace" :size 13 )
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;; Keybindings
(map! "M-s" #'save-buffer :leader)

;;; :editor
;;; evil
(setq evil-split-window-below t
      evil-split-window-right t)


;;; :tools
;; direnv
(setq direnv-always-show-summary nil)

;; magit
(setq magit-repository-directories '(("~/repos" . 2))
      magit-save-repository-buffers nil
      magit-inhibit-save-previous-winconf t)

;;; :lang
;; org
(setq org-directory "~/repos/org/"
      org-archive-location (concat org-directory "archive/%s::")
      org-ellipsis " ▼ ")
(after! org
  (add-to-list 'org-modules 'org-habit t))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
