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

;; -- CC-Mode Customization --

;; Basic formatting for C mode and derivatives
(setq c-default-style "bsd")
(setq c-basic-offset 2)

(defun customize-cc-mode ()
  "Custom hook for `c-mode' and derivatives."
  (local-set-key (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-hook 'customize-cc-mode)
(add-hook 'c++-mode-hook 'customize-cc-mode)

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

;; Key bindings for Magit
(with-eval-after-load "magit"
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; -- MELPA --

;; Enable the MELPA package archive
(with-eval-after-load "package"
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
;; -- File Management --

(defun rename-current-buffer-file (new-file-name)
  "Renames the file associated with the current buffer. Also replaces any
instances of the original local (non-directory) file name in this buffer with
the new local file name."
  (interactive
   (if (not (buffer-file-name (current-buffer)))
       (error "%s is not a file buffer" (buffer-name (current-buffer)))
     (list (read-file-name "Rename to: "))))
  (let ((original-file-name (buffer-file-name (current-buffer))))
    (save-excursion
      (let ((search-string (file-name-nondirectory original-file-name))
	    (replace-string (file-name-nondirectory new-file-name)))
	(goto-char (point-min))
	(while (search-forward search-string nil t)
	  (replace-match replace-string nil t))))
    (write-file new-file-name t)
    (delete-file original-file-name)))

(defun run-on-files (dir fn &optional recursively)
  "Runs the function FN on all files in DIR. If RECURSIVELY is non-nil, the
function will recurse on all sub-directories of DIR as well."
  (dolist (file (directory-files dir))
    (let ((path (concat (file-name-as-directory dir) file)))
      (cond
       ((file-regular-p path)
	(funcall fn path))
       ((and recursively
	     (not (string= "." file))
	     (not (string= ".." file))
	     (file-accessible-directory-p path))
	(run-on-files path fn recursively))))))

(defun run-on-directories (dir fn &optional recursively)
  "Runs the function FN on all directories in DIR (not including DIR itself). If
RECURSIVELY is non-nil, the function will recurse on all sub-directories of DIR
as well."
  (dolist (file (directory-files dir))
    (let ((path (concat (file-name-as-directory dir) file)))
      (when (and (not (string= "." file))
		 (not (string= ".." file))
		 (file-directory-p path))
	(when (and recursively (file-accessible-directory-p path))
	  (run-on-directories path fn recursively))
	(funcall fn path)))))

;; -- Org Mode --

;; Fontify source code blocks in org files by default
(setq org-src-fontify-natively t)

(defun customize-org-mode ()
  "Custom hook for `org-mode'."
  (local-unset-key (kbd "C-,"))
  (local-set-key (kbd "C-c C-<left>") 'insert-left-arrow-char))

(add-hook 'org-mode-hook 'customize-org-mode)

;; -- User Info --

(setq user-full-name "Chris Vig")
(setq user-mail-address "chris@invictus.so")

;; -- MacOS-Specific Setup --

;; Swap default command and option keybindings so my pinkie doesn't fall off
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
