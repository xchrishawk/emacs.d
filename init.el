;;
;; init.el
;; Chris Vig (chris@invictus.so)
;;

;; -- Load Paths --

(let ((local-load-paths (list "~/.emacs.d/lisp")))
  (dolist (path local-load-paths)
    (add-to-list 'load-path path)))

;; -- Package Manager --

(require 'package)
(package-initialize)

;; -- Local Requires --

(require 'local-functions)
(require 'local-setup)

;; -- Third-Party Requires --

(require 'magit nil 'noerror)
