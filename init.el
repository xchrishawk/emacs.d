;; init.el
;; Chris Vig (chris@invictus.so)

;; Load sub-files
(load-file "~/.emacs.d/appearance.el")
(load-file "~/.emacs.d/backups.el")
(load-file "~/.emacs.d/keys.el")
(load-file "~/.emacs.d/layout.el")
(load-file "~/.emacs.d/libs.el")
(load-file "~/.emacs.d/misc.el")

;; Set initial window configuration
(initial-layout)
