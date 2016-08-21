;; local-setup.el
;; Chris Vig (chris@invictus.so)

;; -- Provides --

(provide 'local-setup)

;; -- MELPA --

;; Enable the MELPA package archive
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; -- Key Customization --

;; Opens the buffer list in the current window, instead of the other window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Use C-. and C-, to cycle through windows.
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;; Key bindings to bring up Magit
(when-feature-loaded 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x C-g") 'magit-dispatch-popup))

;; -- Hooks --

;; Add hooks for after initialization
(add-hook 'after-init-hook 'initialize)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- Appearance --

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; Enable column numbers in the info bar for each buffer
(setq column-number-mode t)

;; Turn off menu bar and tool bar, and disable the scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set default font size to 12
(set-font-size 12)

;; Set frame to slightly transparent, and default to maximized
(add-to-list 'default-frame-alist '(alpha . (95 . 70)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load zenburn theme
(load-theme 'zenburn t)

;; -- Formatting --

;; Default auto-fill-mode fill-column to 80 instead of 72
(setq-default fill-column 80)

;; -- Backup Files --

;; Set backup file configuration as suggested at...
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; -- URL Browsing --

;; Set default browser for specific URL categories
(let ((browser-functions nil))
  (push (cons "." 'browse-url-default-browser) browser-functions)
  (when-feature-loaded 'hyperspec
    (push (cons common-lisp-hyperspec-root 'eww-browse-url) browser-functions))
  (setq browse-url-browser-function browser-functions))

;; -- Company Mode --

;; Use C-n and C-p to cycle through options
(when-feature-loaded 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

;; -- IDO Mode --

;;; Enable IDO mode
(ido-mode t)

;; -- Org Mode --

;; Fontify source code blocks in org files by default
(setq org-src-fontify-natively t)

(defun customize-org-mode ()
  "Custom hook for `org-mode'."
  (local-unset-key (kbd "C-,")))

(add-hook 'org-mode-hook 'customize-org-mode)

;; -- Org Babel --

;; Enable Racket in Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)
   (lisp . t)))

;; -- Racket Mode --

(defun customize-racket-mode ()
  "Custom hook for `racket-mode'."
  (local-set-key (kbd "C-c l") 'insert-lambda-char))

(when-feature-loaded 'racket-mode
  (add-hook 'racket-mode-hook 'customize-racket-mode))

;; -- Slime --

(when-feature-loaded 'slime
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;; -- Swift Mode --

;; Open *.swift files in swift-mode
(when-feature-loaded 'swift-mode
  (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode)))

;; -- MacOS-Specific Setup --

;; Swap the default command and option keybindings on macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
