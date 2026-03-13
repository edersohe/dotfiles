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
              scroll-conservatively 101)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

(load-theme 'modus-vivendi-tinted :no-confirm)
(add-to-list 'default-frame-alist '(alpha-background . 97))
(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-15"))
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;;; Built-in Completion
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

;;; Essential Utilities
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package which-key
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

;;; Undo System (Required for Evil to behave nicely)
(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config (undo-fu-session-global-mode))

;;; Evil Mode Ecosystem
;; Note: evil-want-keybinding must be nil BEFORE evil and evil-collection load
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-u-scroll t
      evil-want-C-i-jump t
      evil-undo-system 'undo-fu)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;;; Keybindings (General.el)
(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'my/kill-current-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup t)
  
  ;; Create a definer for your leader key (Space)
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Global Leader Bindings
  (my/leader-keys
    "SPC" '(execute-extended-command :which-key "M-x")
    "ESC" '(keyboard-escape-quit :which-key "escape")

    ;; Buffers & Files
    "b" '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bk" '(my/kill-current-buffer :which-key "kill buffer")
    
    "f" '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save")

    "d" '(:ignore t :which-key "directories")
    "dd" '(dired :which-key "dired")
    "dj" '(dired-jump :which-key "dired jump")

    ;; Vterm
    "t" '(:ignore t :which-key "terminal")
    "tt" '(vterm :which-key "vterm")

    ;; Code (Eglot / Flymake)
    "c" '(:ignore t :which-key "code")
    "cf" '(eglot-format :which-key "format")
    "ca" '(eglot-code-actions :which-key "code actions")
    "cr" '(eglot-rename :which-key "rename")
    "cd" '(eglot-find-declaration :which-key "find decl")
    "ci" '(eglot-find-implementation :which-key "find impl")
    "ct" '(eglot-find-typeDefinition :which-key "find type")
    
    "e" '(:ignore t :which-key "errors")
    "]d" '(flymake-goto-next-error :which-key "next err")
    "[d" '(flymake-goto-prev-error :which-key "prev err")
    "eb" '(flymake-show-diagnostics-buffer :which-key "buffer errs")
    "ep" '(flymake-show-project-diagnostics :which-key "project errs")

    ;; Org
    "o" '(:ignore t :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "capture")
    "ol" '(org-store-link :which-key "store link")

    ;; project.el
    "p" '(:ignore t :which-key "project")
    "pf" '(project-find-file :which-key "find file")
    "pb" '(project-switch-to-buffer :which-key "switch buffer")
    "pp" '(project-switch-project :which-key "switch project")
    "pd" '(project-dired :which-key "dired")
    "pg" '(project-find-regexp :which-key "grep")
    "pr" '(project-query-replace-regexp :which-key "replace")
    "pc" '(project-compile :which-key "compile")
    "pv" '(project-vc-dir :which-key "version control")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    
    ;; vc-git
    "g" '(:ignore t :which-key "git")
    "gs" '(vc-dir :which-key "status")
    "gd" '(vc-diff :which-key "diff")
    "gD" '(vc-root-diff :which-key "diff root")
    "gP" '(vc-push :which-key "push")
    "gl" '(vc-print-log :which-key "log")
    "gb" '(vc-annotate :which-key "blame")
    "gp" '(vc-update :which-key "pull")
    "gn" '(vc-next-action :which-key "next action")
    "]h" '(diff-hl-next-hunk :which-key "next hunk")
    "[h" '(diff-hl-previous-hunk :which-key "prev hunk")
    "gh" '(diff-hl-show-hunk :which-key "show hunk")

    ;; help
    "h" '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hb" '(describe-bindings :which-key "describe bindings")
    "hk" '(describe-key :which-key "describe key")
    "hm" '(describe-mode :which-key "describe mode")
    "hs" '(describe-symbol :which-key "help symbol")))
    
;;; Vterm
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000))

;;; Programming Environments
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-autoshutdown t)
  :config
  (add-to-list 'auto-mode-alist '("\\.exs$" . elixir-ts-mode))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rustup" "run" "stable" "rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs '((elixir-mode elixir-ts-mode heex-ts-mode) . ("expert" "--stdio")))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(setq-default eglot-workspace-configuration
  '(:expert (:workspaceSymbols (:minQueryLength 0))))

(use-package flymake
  :hook (prog-mode . flymake-mode))

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

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t
        rust-mode-treesitter-derive t))

(use-package geiser-guile
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
  :defer t
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
  :bind (:map copilot-completion-map
         ("C-n" . copilot-next-completion)
         ("C-p" . copilot-previous-completion)
         ("TAB" . copilot-accept-completion)))

(use-package org
  :defer t)
