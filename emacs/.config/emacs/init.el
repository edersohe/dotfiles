;;; init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      native-comp-speed 2
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
      completion-styles '(basic flex)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))
      completion-auto-select 'second-tab
      completion-auto-help 'always
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 12
      completion-cycle-threshold 3
      completion-ignore-case t
      completions-detailed t
      completion-show-help nil
      tab-always-indent 'complete)

(global-hl-line-mode 1)

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(fido-vertical-mode t)
(global-completion-preview-mode 1)

(require 'uniquify)

(advice-add 'icomplete-post-command-hook :before #'minibuffer-hide-completions)

(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 800000)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(define-key completion-preview-active-mode-map (kbd "C-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "C-p") #'completion-preview-prev-candidate)

(display-time-mode 1)
(setq display-time-format "%H:%M:%S")

(set-face-attribute 'default nil :font "ZedMono Nerd Font" :weight 'regular ':height 135)
(set-face-attribute 'fixed-pitch nil :font "ZedMono Nerd Font" :height 150)

(load-theme 'modus-vivendi-tritanopia :no-confirm)
;; (add-to-list 'default-frame-alist '(alpha-background . 97))
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting for its name."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'my/kill-current-buffer)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package which-key
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c l f" . eglot-format)
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l d" . xref-find-definitions)
	      ("C-c l r" . xref-find-references)
	      ("C-c l R" . eglot-rename)
	      ("C-c l m" . imenu))
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'auto-mode-alist '
	       ("\\.exs$" . elixir-ts-mode))
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
	       '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
	       '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
	       '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("C-c l ]" . flymake-goto-next-error)
	      ("C-c l [" . flymake-goto-prev-error)
	      ("C-c l e" . flymake-show-diagnostics-buffer)
	      ("C-c l E" . flymake-show-project-diagnostics)))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-language-source-alist
               '(rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package diff-hl
  :ensure t
  :custom (diff-hl-disable-on-remote t)
  :hook
  (after-init . global-diff-hl-mode)
  (after-init . diff-hl-flydiff-mode)
  (after-init . diff-hl-margin-mode))

(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay nil)
  :bind (("C-<return>" . copilot-complete)
	 :map copilot-completion-map
	      ("C-n" . copilot-next-completion)
	      ("C-p" . copilot-previous-completion)
	      ("TAB" . copilot-accept-completion)))

(use-package gptel
  :ensure t
  :init (setq gptel-model 'claude-sonnet-4
	      gptel-default-mode 'org-mode
	      gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind (("C-c RET" . gptel-send)
	 :map gptel-mode-map
	      ("C-c m" . gptel-menu)))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

(use-package rust-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
