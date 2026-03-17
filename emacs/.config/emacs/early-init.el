;;; early-init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

;(add-to-list 'default-frame-alist '(alpha-background . 97))
;(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-16"))
;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

