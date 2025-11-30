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

(require 'uniquify)

(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 800000)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(display-time-mode 1)
(setq display-time-format "%H:%M:%S")

(set-face-attribute 'default nil :font "ZedMono Nerd Font" :weight 'light ':height 135)
(set-face-attribute 'fixed-pitch nil :font "ZedMono Nerd Font" :height 150)

(load-theme 'modus-vivendi-tinted :no-confirm)
;; (add-to-list 'default-frame-alist '(alpha-background . 97))
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

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
  (add-to-list 'auto-mode-alist '
	       ("\\.exs$" . elixir-ts-mode))
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

(use-package gptel
  :ensure t
  :after magit
  :init (setq gptel-model 'claude-sonnet-4
	      gptel-default-mode 'org-mode
	      gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind (("C-c RET" . gptel-send)
	 :map gptel-mode-map
	      ("C-c m" . gptel-menu)))

(use-package gptel-magit
  :ensure t
  :init (setq gptel-magit-model 'gpt-4.1)
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t))

(use-package cape
  :ensure t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package magit
  :ensure t
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

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
  (and (buffer-file-name)
       (not (minibufferp))
       (not buffer-read-only)
       (not (derived-mode-p 'special-mode))))

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
