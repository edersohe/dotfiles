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

(setq-default cursor-type 'bar)

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
	      ("C-c f" . eglot-format)
	      ("C-c a" . eglot-code-actions)
	      ("C-c d" . eglot-find-declaration)
	      ("C-c i" . eglot-find-implementation)
	      ("C-c t" . eglot-find-typeDefinition)
	      ("C-c r" . eglot-rename))
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
	      ("C-c n" . flymake-goto-next-error)
	      ("C-c p" . flymake-goto-prev-error)
	      ("C-c e" . flymake-show-diagnostics-buffer)
	      ("C-c E" . flymake-show-project-diagnostics)))

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

(use-package rust-mode
  :ensure t)

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
	 ("C-x c" . gptel)
	 :map gptel-mode-map
	 ("C-c m" . gptel-menu)))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

(defun my/eat-project ()
  "Create an eat buffer and rename it interactively."
  (interactive)
  (eat-project)  ; Call the original eat command
  (when-let* ((project (project-current))
              (project-name (file-name-nondirectory
                             (directory-file-name
                              (project-root project))))
              (eat-buffer (get-buffer (format "*%s-eat*" project-name))))
    (with-current-buffer eat-buffer
      (let ((new-name (read-string "Enter name for eat buffer: ")))
        (rename-buffer (format "*%s-eat-%s*" project-name new-name) t)))))

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "n") 'next-line)
  (define-key view-mode-map (kbd "p") 'previous-line)
  (define-key view-mode-map (kbd "b") 'backward-char)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "e") 'end-of-line)
  (define-key view-mode-map (kbd "a") 'beginning-of-line)
  (define-key view-mode-map (kbd "w") 'kill-ring-save)
  (define-key view-mode-map (kbd "v") 'scroll-up-command)
  (define-key view-mode-map (kbd "t") 'my/eat-project)
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
