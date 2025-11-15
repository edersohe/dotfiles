;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default
 inhibit-startup-screen t      ;; Disable the startup screen
 initial-scratch-message nil)  ;; Tidy up the scratch buffer

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))
