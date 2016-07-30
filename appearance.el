;; appearance.el
;; Chris Vig (chris@invictus.so)

;; Don't display splash screen at startup
(setq inhibit-startup-screen t)

;; Disable menu bar, scroll bar, and tool bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Add and load zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
(load-theme 'zenburn t)
