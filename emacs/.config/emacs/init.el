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
  (setq gptel-model 'claude-sonnet-4
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

(setq-default cursor-type 'bar)
(with-eval-after-load 'view
  (defvar view-mode-leader-map (make-sparse-keymap)
    "Keymap for view-mode leader keys starting with SPC.")
  
  (define-key view-mode-map (kbd "SPC") view-mode-leader-map)

  (define-key view-mode-leader-map (kbd "p") 'project-switch-project)
  (define-key view-mode-leader-map (kbd "d") 'project-dired)
  (define-key view-mode-leader-map (kbd "v") 'project-vc-dir)
  (define-key view-mode-leader-map (kbd "f") 'project-find-file)
  (define-key view-mode-leader-map (kbd "b") 'project-switch-to-buffer)
  (define-key view-mode-leader-map (kbd "t") 'my/eat-project)
  (define-key view-mode-leader-map (kbd "h") 'describe-symbol)
  (define-key view-mode-leader-map (kbd "k") 'describe-key)
  (define-key view-mode-leader-map (kbd "C-g") 'keyboard-quit)

  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "n") 'next-line)
  (define-key view-mode-map (kbd "p") 'previous-line)
  (define-key view-mode-map (kbd "b") 'backward-char)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "e") 'end-of-line)
  (define-key view-mode-map (kbd "a") 'beginning-of-line)
  (define-key view-mode-map (kbd "w") 'kill-ring-save)
  (define-key view-mode-map (kbd "v") 'scroll-up-command)
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
       (not (derived-mode-p 'special-mode)))
  (or (derived-mode-p 'prog-mode)
      (derived-mode-p 'fundamental-mode)
      (derived-mode-p 'markdown-mode)
      (derived-mode-p 'org-mode)
      (derived-mode-p 'help-mode)))

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
