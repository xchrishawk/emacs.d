;;
;; init.el
;; Chris Vig (chris@invictus.so)
;;

;; -- Load Paths --

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; -- Package Manager --

(require 'package)
(package-initialize)

;; -- Local Requires --

(require 'local-functions)
(require 'local-setup)
(require 'make)

;; -- Third-Party Requires --

(require 'cmake-mode nil 'noerror)
(require 'company nil 'noerror)
(require 'elpy nil 'noerror)
(require 'flycheck nil 'noerror)
(require 'magit nil 'noerror)
(require 'irony nil 'noerror)
