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
      load-prefer-newer t
      completion-styles '(basic flex)
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
;(global-hl-line-mode 1)

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
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(define-key completion-preview-active-mode-map (kbd "C-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "C-p") #'completion-preview-prev-candidate)

(display-time-mode 1)
(setq display-time-format "%H:%M:%S")

(set-face-attribute 'default nil :font "D2CodingLigature Nerd Font" :weight 'light ':height 135)
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
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c l f" . eglot-format)
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l d" . xref-find-definitions)
	      ("C-c l D" . xref-find-declarations)
	      ("C-c l r" . xref-find-references)
	      ("C-c l R" . eglot-rename)
	      ("C-c l ]" . flymake-goto-next-error)
	      ("C-c l [" . flymake-goto-prev-error)
	      ("C-c l e" . flymake-show-diagnostics-buffer)
	      ("C-c l E" . flymake-show-project-diagnostics)
	      ("C-c l m" . imenu))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
	       '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
	       '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
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

(defun my/gptel-smart-send ()
  "Execute 'gptel-send' if text is selected, otherwise execute 'gptel'."
  (interactive)
  (if (use-region-p)
      (gptel-send t)
    (gptel "*Copilot*" nil nil t)))

(use-package gptel
  :ensure t
  :init (setq gptel-model 'claude-sonnet-4
	      gptel-default-mode 'markdown-mode
	      gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind (("C-c RET" . my/gptel-smart-send)
	 :map gptel-mode-map
	      ("C-c m" . gptel-menu)))

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom (aidermacs-default-chat-mode 'code))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "n") 'next-line)
  (define-key view-mode-map (kbd "p") 'previous-line)
  (define-key view-mode-map (kbd "b") 'backward-word)
  (define-key view-mode-map (kbd "f") 'forward-word)
  (define-key view-mode-map (kbd "e") 'end-of-line)
  (define-key view-mode-map (kbd "a") 'beginning-of-line)
  (define-key view-mode-map (kbd "w") 'kill-ring-save)
  (define-key view-mode-map (kbd "V") 'scroll-down-command)
  (define-key view-mode-map (kbd "v") 'scroll-up-command)
  (define-key view-mode-map (kbd "SPC") 'set-mark-command)
  (define-key view-mode-map (kbd "RET") nil)
  (define-key view-mode-map (kbd "DEL") nil)
  (define-key view-mode-map (kbd "0") 'delete-window)
  (define-key view-mode-map (kbd "1") 'delete-other-windows)
  (define-key view-mode-map (kbd "2") 'split-window-below)
  (define-key view-mode-map (kbd "3") 'split-window-right)
  (define-key view-mode-map (kbd "o") 'other-window))

(add-hook 'view-mode-hook
	  (lambda ()
	    (setq cursor-type (if view-mode 'box 'bar))))

(defun my/should-enable-view-mode-p ()
  "Return t if view-mode should be enabled in current buffer."
  (and (not (minibufferp))
       (not buffer-read-only)
       (derived-mode-p 'prog-mode)))
  
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (my/should-enable-view-mode-p)
              (view-mode 1))))

(defadvice keyboard-quit (around my/keyboard-quit-advice activate)
  "Enable view-mode instead of quitting when in a major mode buffer."
  (if (and (my/should-enable-view-mode-p)
           (not view-mode))
      (view-mode 1)
    ad-do-it))

(global-set-key (kbd "C-c v") 'view-mode)

(provide 'init)
;;; init.el ends here
