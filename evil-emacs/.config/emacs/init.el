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
              scroll-conservatively 101)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

(add-to-list 'default-frame-alist '(alpha-background . 97))
(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-14"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(repeat-mode t)
(global-auto-revert-mode t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

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

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'my/kill-current-buffer)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-tokyo-night :no-confirm))

(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-allow-imprecise-window-fit nil)
  :config
  (which-key-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

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

(global-set-key (kbd "C-x c v") #'my/vterm)

(use-package eglot
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
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c d n" . flymake-goto-next-error)
              ("C-c d p" . flymake-goto-prev-error)
              ("C-c d b" . flymake-show-diagnostics-buffer)
              ("C-c d a" . flymake-show-project-diagnostics)))

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
  (after-init . diff-hl-margin-mode)
  :bind (:map diff-hl-mode-map
              ("C-c h n" . diff-hl-next-hunk)
              ("C-c h p" . diff-hl-previous-hunk)
              ("C-c h s" . diff-hl-show-hunk)))

(use-package magit
  :ensure t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package org
  :init
  (defun my/org-open-life ()
    "Open the main org file for life management."
    (interactive)
    (find-file "~/notes/life.org"))
  :bind (("C-x c l" . org-store-link)
         ("C-x c a" . org-agenda)
         ("C-x c c" . org-capture)
         ("C-x c o" . my/org-open-life))
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
  :bind (("C-<return>" . copilot-complete)
         :map copilot-completion-map
         ("C-n" . copilot-next-completion)
         ("C-p" . copilot-previous-completion)
         ("TAB" . copilot-accept-completion)))

(use-package eca
  :ensure t
  :custom
  (eca-completion-idle-delay nil)
  :bind (("C-x c e" . eca)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package corfu
  :ensure t
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t))

(use-package cape
  :ensure t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package evil
  :ensure t
  :after undo-fu
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
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

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package evil-matchit
  :ensure
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup t)
  :init
  (general-define-key
   :states '(normal visual)
   :keymaps 'global
   :prefix "SPC"
   "p"  '(project-switch-project :which-key "switch project")
   "b"  '(project-switch-to-buffer :which-key "switch buffer")
   "B"  '(switch-to-buffer :which-key "switch buffer")
   "f"  '(project-find-file :which-key "find file")
   "F"  '(find-file :which-key "find file")
   "e"  '(project-dired :which-key "explore")
   "E"  '(dired :which-key "dired")
   "g"  '(magit-status :which-key "magit status")
   "h"  '(diff-hl-show-hunk :which-key "show hunk")
   "s"  '(project-query-replace-regexp :which-key "replace")
   "t"  '(my/vterm :which-key "terminal")
   "T"  '(eat :which-key "terminal")
   "RET" '(gptel-menu t :which-key "gptel menu")
   "c"  '(gptel :which-key "chat")
   "?"  '(describe-symbol :which-key "help")
   "k"  '(describe-key :which-key "describe key")
   "K"  '(describe-bindings :which-key "keybindings")
   "i"  '(imenu :which-key "imenu")
   "/"  '(project-find-regexp :which-key "find")
   "c"  '(org-capture :which-key "capture")
   "a"  '(org-agenda :which-key "agenda")
   "w"  '(save-buffer :which-key "save"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'eglot-mode-map
   :prefix "SPC"
   "l"  '(:ignore t :which-key "lsp")
   "lf" '(eglot-format :which-key "format")
   "la" '(eglot-code-actions :which-key "actions")
   "lr" '(eglot-rename :which-key "rename")
   "ld" '(eglot-find-declaration :which-key "declaration")
   "li" '(eglot-find-implementation :which-key "implementation")
   "lt" '(eglot-find-typeDefinition :which-key "type definition"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'flymake-mode-map
   :prefix "SPC"
   "d" '(flymake-show-project-diagnostics :which-key "diagnostics"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'flymake-mode-map
   :prefix nil
   "]d" '(flymake-goto-next-error :which-key "next diagnostic")
   "[d" '(flymake-goto-prev-error :which-key "previous diagnostic"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'diff-hl-mode-map
   :prefix nil
   "]h" '(diff-hl-next-hunk :which-key "next hunk")
   "[h" '(diff-hl-previous-hunk :which-key "previous hunk")))

(provide 'init)
;;; init.el ends here
