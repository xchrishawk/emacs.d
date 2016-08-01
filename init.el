;; init.el
;; Chris Vig (chris@invictus.so)

;; ---- Includes ----

;; Load other configuration files
(load-file "~/.emacs.d/keys.el")
(load-file "~/.emacs.d/layout.el")

;; ---- Appearance ----

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; Enable column numbers in the info bar for each buffer
(setq column-number-mode t)

;; Turn off menu bar and tool bar, and disable the scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set default font size to 12
(set-face-attribute 'default nil :height 120)

;; Set frame to slightly transparent, and default to maximized
(add-to-list 'default-frame-alist '(alpha . (95 . 70)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
(load-theme 'zenburn t)

;; ---- Backup Files ----

;; Set backup file configuration as suggested at...
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs.backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ---- IDO Mode ----

;;; Enable IDO mode
(require 'ido)
(ido-mode t)

;; ---- MELPA ----

;; Enable the MELPA package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; ---- Key Customization ----

;; Opens the buffer list in the current window, instead of the other window.
(global-set-key "\C-x\C-b" 'buffer-menu)

;; ---- Hooks ----

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
