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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(electric-pair-mode t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(require 'uniquify)

(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 800000)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'flymake-mode)

;; Maximize for floating window manager
;;(add-to-list 'initial-frame-alist '(fullscreen . fullboth))
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(display-time-mode 1)
(setq display-time-format "%H:%M:%S")

(set-face-attribute 'default nil :font "Hasklug Nerd Font"
		    :height 110)
(set-face-attribute 'fixed-pitch nil :font "Hasklug Nerd Font"
		    :height 120)

(load-theme 'modus-vivendi-tinted :no-confirm)

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

(use-package catppuccin-theme
  :ensure
  t
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package doom-modeline
  :ensure
  t
  :init
  (doom-modeline-mode 1))

(use-package orderless
  :ensure
  t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure
  t
  :after
  orderless
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package marginalia
  :ensure
  t
  :after
  vertico
  :config
  (marginalia-mode))

(use-package corfu
  :ensure
  t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

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
  "Execute copilot-complete if no region is selected, otherwise execute gptel-send."
  (interactive)
  (if (use-region-p)
      (gptel-send)
    (copilot-complete)))

(global-set-key (kbd "C-<return>") 'smart-ai-complete)

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
  ("<tab>" . copilot-accept-completion-by-line)))

(use-package gptel
  :ensure
  t
  :init
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'claude-sonnet-4))

(use-package gptel-magit
  :ensure
  t
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install))

(provide 'init)
;;; init.el ends here
