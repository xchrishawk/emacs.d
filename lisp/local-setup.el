;;
;; local-setup.el
;; Chris Vig (chris@invictus.so)
;;

;; -- Provides --

(provide 'local-setup)

;; -- Appearance --

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; Enable column numbers in the info bar
(setq column-number-mode t)

;; Turn off menu bar, tool bar, and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set frame to slightly transparent, and default to maximized
(add-to-list 'default-frame-alist '(alpha . (95 . 70)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load the zenburn theme
(with-eval-after-load "package"
  (load-theme 'zenburn t))

;; -- Backup Files --

;; Set backup file configuration as suggested at...
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; -- Customization File --

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; -- Enable Functions --

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; -- Formatting --

;; Default auto-fill-mode fill-column to 80 instead of 72
(setq-default fill-column 80)

;; Never use tabs
(setq-default indent-tabs-mode nil)

;; -- Global Hooks --

;; Run `initialize' on startup
(add-hook 'after-init-hook 'initialize)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- IDO Mode --

;; Enable IDO mode
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

;; -- Key Bindings --

;; Open buffer list in current window, instead of other window
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Use C-, and C-. to cycle through other windows
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;; -- MELPA --

;; Enable the MELPA package archive
(with-eval-after-load "package"
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

;; -- Org Mode --

;; Fontify source code blocks in org files by default
(setq org-src-fontify-natively t)

(defun customize-org-mode ()
  "Custom hook for `org-mode'."
  (local-unset-key (kbd "C-,"))
  (local-set-key (kbd "C-c C-<left>") 'insert-left-arrow-char))

(add-hook 'org-mode-hook 'customize-org-mode)

;; -- MacOS-Specific Setup --

;; Swap default command and option keybindings so my pinkie doesn't fall off
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
