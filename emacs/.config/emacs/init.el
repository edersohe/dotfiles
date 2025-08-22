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
      load-prefer-newer t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(hl-line-mode 1)

(electric-pair-mode t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(require 'uniquify)

(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 800000)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'flymake-mode)

;; Enable this when use a traditional Desktop Environment instead Tiling Window Manager
;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

(display-time-mode 1)
(setq display-time-format "%H:%M:%S")

(set-face-attribute 'default nil :font "D2CodingLigature Nerd Font"
		    :weight 'light ':height 140)
(set-face-attribute 'fixed-pitch nil :font
		    "D2CodingLigature Nerd Font"
		    :height 160)

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting for its name."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'my/kill-current-buffer)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode))

(use-package eglot
  :hook
  (prog-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
	       '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
	       '(elixir-mode . ("elixir-ls" ""))))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(use-package exec-path-from-shell
  :ensure
  t
  :if
  (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package catppuccin-theme
  :ensure
  t
  :config
  (load-theme 'catppuccin :no-confirm)
  (add-to-list 'default-frame-alist '(alpha-background . 98)))

(use-package doom-modeline
  :ensure
  t
  :init
  (doom-modeline-mode 1))

(use-package orderless
  :ensure
  t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure
  t
  :after
  orderless
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package marginalia
  :ensure
  t
  :after
  vertico
  :config
  (marginalia-mode))

(use-package corfu
  :ensure
  t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package treesit-auto
  :ensure
  t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :ensure
  t)

(use-package diff-hl
  :ensure
  t
  :after magit
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode t)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(defun smart-ai-complete ()
  "Execute 'copilot-complete' if no region is selected, otherwise execute 'gptel-send'."
  (interactive)
  (minibuffer-hide-completions)
  (if (use-region-p)
      (gptel-send)
    (copilot-complete)))

(global-set-key (kbd "C-<return>") 'smart-ai-complete)

(use-package copilot
  :ensure
  t
  :hook
  (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay nil)
  :bind
  (:map copilot-completion-map
  ("C-n" . copilot-next-completion)
  ("C-p" . copilot-previous-completion)
  ("<tab>" . copilot-accept-completione)))

(use-package gptel
  :ensure
  t
  :init
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'claude-sonnet-4))

(use-package gptel-magit
  :ensure
  t
  :hook
  (magit-mode . gptel-magit-install))

(use-package eat
  :ensure
  t
  :hook
  (eshell-load . eat-eshell-mode))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :ensure
  t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :ensure
  t
  :after
  meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'init)
;;; init.el ends here
