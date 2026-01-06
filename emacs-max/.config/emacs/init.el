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
             :bind (("C-<return>" . copilot-complete)
                    :map copilot-completion-map
                    ("C-n" . copilot-next-completion)
                    ("C-p" . copilot-previous-completion)
                    ("TAB" . copilot-accept-completion)))

(use-package gptel
             :ensure t
             :defer t
             :init
             (setq gptel-model 'gemini-3-flash-preview
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

(use-package undo-fu
             :ensure t)

(use-package undo-fu-session
             :ensure t
             :after undo-fu
             :config
             (undo-fu-session-global-mode))

(use-package magit
             :ensure t
             :defer t
             :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package gptel-magit
             :ensure t
             :defer t
             :init (setq gptel-magit-model 'gpt-4.1)
             :after (gptel magit)
             :hook (magit-mode . gptel-magit-install))

(setq-default cursor-type 'bar)
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
