;;; init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold (* 100 1024 1024)
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      native-comp-async-report-warnings-errors nil
      native-comp-speed 2
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      inhibit-startup-screen t
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
      load-prefer-newer t)

(setq completion-styles '(basic flex)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))
      completion-auto-select 'second-tab
      completion-auto-help 'always
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 12
      completion-ignore-case t
      completions-detailed t
      completion-show-help nil
      tab-always-indent 'complete)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(hl-line-mode 1)

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(fido-vertical-mode t)
(global-completion-preview-mode)

(require 'uniquify)

(advice-add 'icomplete-post-command-hook :before #'minibuffer-hide-completions)

(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 800000)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(define-key completion-preview-active-mode-map (kbd "C-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "C-p") #'completion-preview-prev-candidate)

(display-time-mode 1)
(setq display-time-format "%H:%M:%S")

(set-face-attribute 'default nil :font "D2CodingLigature Nerd Font" :weight 'semi-light ':height 135)
(set-face-attribute 'fixed-pitch nil :font "D2CodingLigature Nerd Font" :height 150)

(load-theme 'modus-vivendi-tinted)
(add-to-list 'default-frame-alist '(alpha-background . 98))
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting for its name."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'my/kill-current-buffer)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode))

(use-package eglot
  :hook
  (prog-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
	       '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
	       '(elixir-mode . ("elixir-ls" ""))))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(use-package exec-path-from-shell
  :ensure
  t
  :if
  (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package treesit-auto
  :ensure
  t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :ensure
  t)

(use-package diff-hl
  :ensure
  t
  :after magit
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode t)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(defun smart-ai-complete ()
  "Execute 'copilot-complete' if no region is selected, otherwise execute 'gptel-send'."
  (interactive)
  (minibuffer-hide-completions)
  (if (use-region-p)
      (gptel-send)
    (copilot-complete)))

(global-set-key (kbd "M-RET") 'smart-ai-complete)

(use-package copilot
  :ensure
  t
  :hook
  (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay nil)
  :bind
  (:map copilot-completion-map
  ("C-n" . copilot-next-completion)
  ("C-p" . copilot-previous-completion)
  ("TAB" . copilot-accept-completion)))

(use-package gptel
  :ensure
  t
  :init
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'claude-sonnet-4))

(use-package gptel-magit
  :ensure
  t
  :hook
  (magit-mode . gptel-magit-install))

(use-package eat
  :ensure
  t
  :hook
  (eshell-load . eat-eshell-mode))

(provide 'init)
;;; init.el ends here
