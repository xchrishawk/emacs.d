;; init.el
;; Chris Vig (chris@invictus.so)

;; -- Load Paths --

;; Set up load path
(let ((local-load-paths
       (list "~/.emacs.d/lisp"
	     "~/.emacs.d/lisp/ascii-table"
	     "~/.emacs.d/lisp/ob-mixal"
	     "~/.emacs.d/lisp/ob-racket")))
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

;; Load 3rd-party modules
(require 'company nil t)
(require 'hyperspec nil t)
(require 'ido nil t)
(require 'magit nil t)
(require 'package nil t)

;; Load modules which are included as Git repos under lisp/
(require 'ascii-table)

;; Do initial setup
(require 'local-functions)
(require 'local-make)
(require 'local-setup)
