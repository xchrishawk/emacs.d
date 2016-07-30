;; emacs init file
;; Chris Vig (chris@invictus.so)

;; Don't display splash screen at startup
(setq inhibit-startup-screen t)

;; Disable menu bar, scroll bar, and tool bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Set up backup file creation
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs.backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Add and load zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
(load-theme 'zenburn t)
