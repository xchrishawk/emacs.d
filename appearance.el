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

;; Set frame transparency
(add-to-list 'default-frame-alist '(alpha . (95 . 70)))

;; Default initial frame to maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable column numbers by default
(setq column-number-mode t)
