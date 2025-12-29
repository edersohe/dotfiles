;;; init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:


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
  "Kill the current buffer without prompting for its name."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'my/kill-current-buffer)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package exec-path-from-shell
  :ensure t
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
               '((rust-mode rust-ts-mode) . ("rustup" "run" "stable" "rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c e" . flymake-show-diagnostics-buffer)
              ("C-c E" . flymake-show-project-diagnostics)))

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
  :bind (("C-<return>" . copilot-complete)
         :map copilot-completion-map
         ("C-n" . copilot-next-completion)
         ("C-p" . copilot-previous-completion)
         ("TAB" . copilot-accept-completion)))

(use-package gptel
  :ensure t
  :defer t
  :init
  (setq gptel-model 'claude-sonnet-4
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind (("C-c RET" . gptel-send)
         ("C-x c" . gptel)
         :map gptel-mode-map
         ("C-c m" . gptel-menu)))

(use-package gptel-magit
  :ensure t
  :defer t
  :init (setq gptel-magit-model 'gpt-4.1)
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install))

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

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode)
  :bind ("C-x p t" . my/eat-project))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :after evil-collection
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

(use-package magit
  :ensure t
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package undo-fu
  :ensure t)

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
   "f"  '(project-find-file :which-key "find file")
   "e"  '(project-dired :which-key "explore")
   "g"  '(magit-status :which-key "magit status")
   "s"  '(project-query-replace-regexp :which-key "replace")
   "t"  '(my/eat-project :which-key "terminal")
   "RET" '(gptel-menu t :which-key "gptel menu")
   "c"  '(gptel :which-key "chat")
   "h"  '(describe-symbol :which-key "help")
   "k"  '(describe-key :which-key "key help")
   "i" '(imenu :which-key "imenu"))
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
   "d" '(flymake-show-project-diagnostics :which-key "diagnostics")))

(provide 'init)
;;; init.el ends here
