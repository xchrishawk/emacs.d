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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
