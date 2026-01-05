;;; early-init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory)
	          gc-cons-threshold most-positive-fixnum
	          inhibit-startup-screen t
	          initial-scratch-message nil
              initial-major-mode 'fundamental-mode
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
              visible-bell t)

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(load-theme 'modus-vivendi-tinted :no-confirm)
(add-to-list 'default-frame-alist '(alpha-background . 97))
(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-13.5"))
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(require 'package)
(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 800000)))

(provide 'early-init)
;;; early-init.el ends here
