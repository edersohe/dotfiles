;;; init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-install-upgrade-built-in t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory)
              make-backup-files nil
              auto-save-default nil
              create-lockfiles nil
              indent-tabs-mode nil
              tab-width 4
              blink-cursor-mode t
              use-short-answers t
              use-dialog-box nil
              password-cache-expiry nil
              confirm-kill-emacs nil
              uniquify-buffer-name-style 'forward
              load-prefer-newer t
              help-window-select t
              native-comp-async-report-warnings-errors nil
              native-comp-speed 3
              project-mode-line t
              scroll-conservatively 101
              elisp-flymake-byte-compile nil
              tramp-connection-timeout 10
              tramp-use-ssh-controlmaster-options nil
              native-comp-deferred-compilation t)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

(add-to-list 'default-frame-alist '(alpha-background . 93))
(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-14"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(repeat-mode 1)
(global-auto-revert-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(fido-vertical-mode 1)
(load-theme 'modus-vivendi-tinted :no-confirm)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setq completion-styles '(basic flex partial-completion)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))
      completion-auto-select 'second-tab
      completion-auto-help nil
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 12
      completion-cycle-threshold 2
      completion-ignore-case t
      completions-detailed t
      completion-show-help nil
      tab-always-indent 'complete)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-allow-imprecise-window-fit nil)
  :config (which-key-mode 1))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config (undo-fu-session-global-mode))

(use-package vterm
  :ensure t
  :custom (vterm-max-scrollback 10000))

(defun my/vterm ()
  "Open a new vterm buffer with a custom name."
  (interactive)
  (let ((name (read-string "Name for vterm buffer: ")))
    (vterm (concat "*vterm-" name "*"))))

(global-set-key (kbd "C-c v") #'my/vterm)

(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (json "https://github.com/tree-sitter/tree-sitter-json")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (go-mode . go-ts-mode)
        (js-mode . js-ts-mode)
        (c-mode . c-ts-mode)
        (mhtml-mode . html-ts-mode)
        (css-mode . css-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (php-mode . php-ts-mode)
        (java-mode . java-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (toml-mode . toml-ts-mode)
        (js-json-mode . json-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

(use-package eglot
  :hook ((python-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (elixir-ts-mode . eglot-ensure)
         (heex-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (html-ts-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure)
         (php-ts-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         ;; Uncomment if you install these via MELPA:
         ;; (kotlin-mode . eglot-ensure)
         ;; (dart-mode . eglot-ensure)
         )
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
  (add-to-list 'eglot-server-programs '((python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '((rust-ts-mode) . ("rustup" "run" "stable" "rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs '((elixir-ts-mode heex-ts-mode) . ("expert" "--stdio")))
  (add-to-list 'eglot-server-programs '((ruby-ts-mode) . ("ruby-lsp"))))

(setq-default eglot-workspace-configuration
              '(:expert (:workspaceSymbols (:minQueryLength 0))))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c d" . flymake-show-buffer-diagnostics)
              ("C-c D" . flymake-show-project-diagnostics)))

(use-package markdown-mode
  :ensure t
  :custom (markdown-fontify-code-blocks-natively t)
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package geiser-guile
  :ensure t)

(use-package diff-hl
  :ensure t
  :custom (diff-hl-disable-on-remote t)
  :hook
  (after-init . global-diff-hl-mode)
  (after-init . diff-hl-flydiff-mode)
  (after-init . diff-hl-margin-mode)
  :bind (:map diff-hl-mode-map
              ("C-c h n" . diff-hl-next-hunk)
              ("C-c h p" . diff-hl-previous-hunk)
              ("C-c h s" . diff-hl-show-hunk)))

(use-package org
  :init
  (defun my/org-open-life ()
    "Open the main org file for life management."
    (interactive)
    (find-file "~/notes/life.org"))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c o" . my/org-open-life))
  :custom
  (org-default-notes-file "~/notes/life.org")
  (org-agenda-files '("~/notes/life.org"))
  (org-log-done 'time)
  (org-hide-leading-stars t)
  :config
  (setq org-capture-templates
        '(("t" "Task / Meeting" entry (file+headline "~/notes/life.org" "Actionable")
           "* TODO %^{Title/Subject} %^g\n  %^{When|SCHEDULED|DEADLINE}: %^t\n\n  *Notes / Details:*\n  %?\n\n  *Action Items / Subtasks:*\n  - [ ]\n"
           :empty-lines 1)
          ("n" "Timeless Note" entry (file+headline "~/notes/life.org" "Reference")
           "* %^{Title} %^g\n  Captured: %U\n\n  %?\n"
           :empty-lines 1)))
  (setq org-agenda-custom-commands
        '(("w" "Work Dashboard" tags "+work")
          ("p" "Personal Dashboard" tags "+personal"))))

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

(use-package epa
  :custom
  (epa-file-select-keys nil)
  (epa-pinentry-mode 'loopback)
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package gnus
  :bind (("C-c m" . gnus))
  :custom
  (gnus-select-method
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port "imaps")
            (nnimap-stream ssl)))
  (gnus-secondary-select-methods '((nnrss "RSS")))
  (gnus-message-replysign t)
  (gnus-message-replyencrypt t)
  (mm-verify-option 'known)
  (mm-decrypt-option 'known)
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls))

(defun my/vc-next-action ()
  "'vc-next-action with auto generated commit message."
  (interactive)
  (if (string-equal (buffer-name) "*vc-diff*")
      (let ((output (shell-command-to-string
                     (format "commit-message-generator %s"
                             (shell-quote-argument (buffer-string))))))
        (vc-next-action nil)
        (if (string-equal (buffer-name) "*vc-log*")
            (insert output)))
    (vc-next-action nil)))
(define-key vc-prefix-map (kbd "v") #'my/vc-next-action)

(provide 'init)
;;; init.el ends here
