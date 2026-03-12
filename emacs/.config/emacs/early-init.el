;;; early-init.el --- Customization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(provide 'early-init)
;;; early-init.el ends here
