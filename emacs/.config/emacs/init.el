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
(global-completion-preview-mode 1)
(define-key completion-preview-active-mode-map (kbd "C-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "C-p") #'completion-preview-prev-candidate)

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
  (eglot-events-buffer-config '(:size 0 :format full))
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
  (copilot-idle-delay nil)
  :bind (("C-<return>" . copilot-complete)
         :map copilot-completion-map
         ("C-n" . copilot-next-completion)
         ("C-p" . copilot-previous-completion)
         ("TAB" . copilot-accept-completion)))

(use-package gptel
  :ensure t
  :defer t
  :init
  (setq gptel-model 'gemini-3-pro-preview
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind (("C-c RET" . gptel-send)
         ("C-x c" . gptel)
         :map gptel-mode-map
         ("C-c m" . gptel-menu)))

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

(provide 'init)
;;; init.el ends here
