;; init.el
;; Chris Vig (chris@invictus.so)

;; -- Load Paths --

;; Set up load path
(let ((local-load-paths
       (list "~/.emacs.d/lisp"
	     "~/.emacs.d/lisp/ascii-table"
	     "~/.emacs.d/lisp/ob-racket"
	     "~/.emacs.d/lisp/swift-mode"
	     "~/.emacs.d/lisp/zenburn")))
  (dolist (path local-load-paths)
    (add-to-list 'load-path path)))

;; Set up theme path
(let ((local-theme-paths
       (list "~/.emacs.d/lisp/zenburn-emacs")))
  (dolist (path local-theme-paths)
    (add-to-list 'custom-theme-load-path path)))

;; -- Packages --

;; Set up package manager
(require 'package)
(package-initialize)

;; -- Requires --

;; Do initial setup
(require 'local-functions)
(require 'local-setup)

;; Load modules
(require 'ascii-table)
(require 'swift-mode)
