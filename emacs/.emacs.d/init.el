;; Don't show welcome splash
(setq inhibit-startup-message t)

;; Visible event when wrong action
(setq visible-bell t)

;; Disable the tool-bar
(tool-bar-mode -1)

;; Disable the scroll-bar
(scroll-bar-mode -1)

;; Disable menu-bar
(menu-bar-mode -1)

; Give some breathing room
(set-fringe-mode 10)

;; Show line numbers
(global-display-line-numbers-mode 1)

;; Set emacs color theme
(load-theme 'modus-vivendi t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Stop creating backup~ files
(setq make-backup-files nil)

;; Stop creating #autosave# files
(setq auto-save-default nil) 

;; Enable highlighting lines
(hl-line-mode nil)

;; Remember most recent files used inside emacs
(recentf-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
