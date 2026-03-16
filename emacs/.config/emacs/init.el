;;; init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory)
              make-backup-files nil
              auto-save-default nil
              create-lockfiles nil
              indent-tabs-mode nil
              tab-width 4
              blink-cursor-mode nil
              use-short-answers t
              use-dialog-box nil
              password-cache-expiry nil
              confirm-kill-emacs nil
              uniquify-buffer-name-style 'forward
              window-resize-pixelwise t
              frame-resize-pixelwise t
              load-prefer-newer t
              help-window-select t
              native-comp-async-report-warnings-errors nil
              native-comp-speed 3
              project-mode-line t
              scroll-conservatively 101
              use-package-always-ensure t)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

(add-to-list 'default-frame-alist '(alpha-background . 97))
(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-15"))
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(setq completion-styles '(basic flex)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))
      completion-auto-select 'second-tab
      completion-auto-help nil
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 12
      completion-cycle-threshold 5
      completion-ignore-case t
      completions-detailed t
      completion-show-help nil
      tab-always-indent 'complete)
(fido-vertical-mode t)
(global-completion-preview-mode 1)
(define-key completion-preview-active-mode-map (kbd "C-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "C-p") #'completion-preview-prev-candidate)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package catppuccin-theme
  :config (load-theme 'catppuccin :no-confirm))

(use-package which-key
  :ensure nil
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

(use-package marginalia
  :config (marginalia-mode))

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :config (undo-fu-session-global-mode))

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'my/kill-current-buffer)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package vterm
  :custom (vterm-max-scrollback 10000))

(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c l f" . eglot-format)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l r" . eglot-rename))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-autoshutdown t)
  :config
  (add-to-list 'auto-mode-alist '("\\.exs$" . elixir-ts-mode))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rustup" "run" "stable" "rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs '((elixir-mode elixir-ts-mode heex-ts-mode) . ("expert" "--stdio")))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("ruby-lsp"))))

(setq-default eglot-workspace-configuration
  '(:expert (:workspaceSymbols (:minQueryLength 0))))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c d n" . flymake-goto-next-error)
              ("C-c d p" . flymake-goto-prev-error)
              ("C-c d b" . flymake-show-diagnostics-buffer)
              ("C-c d a" . flymake-show-project-diagnostics)))

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :init
  (setq rust-format-on-save t
        rust-mode-treesitter-derive t))

(use-package geiser-guile)

(use-package diff-hl
  :custom (diff-hl-disable-on-remote t)
  :hook
  (after-init . global-diff-hl-mode)
  (after-init . diff-hl-flydiff-mode)
  (after-init . diff-hl-margin-mode)
  :bind (:map diff-hl-mode-map
              ("C-c h n" . diff-hl-next-hunk)
              ("C-c h p" . diff-hl-previous-hunk)
              ("C-c h s" . diff-hl-show-hunk)))

(use-package copilot
  :defer t
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay nil)
  :bind (("C-<return>" . copilot-complete)
         :map copilot-completion-map
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)
              ("TAB" . copilot-accept-completion)))

(use-package magit
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package gptel
  :defer t
  :init
  (setq gptel-model 'gpt-5-mini
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind (("C-c RET" . gptel-send)
         ("C-x c" . gptel)
         :map gptel-mode-map
         ("C-c m" . gptel-menu)))

(use-package gptel-magit
  :defer t
  :init (setq gptel-magit-model 'gpt-5-mini)
  :hook (magit-mode . gptel-magit-install))

(use-package org
  :ensure nil
  :defer t
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)))

(use-package diminish
  :defer t
  :init
  (diminish 'which-key-mode)
  (diminish 'completion-preview-mode)
  (diminish 'eldoc-mode))
