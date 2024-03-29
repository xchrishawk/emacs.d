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
(add-to-list 'default-frame-alist '(alpha . (98 . 85)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable current line highlighting
(global-hl-line-mode nil)

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
  (local-set-key (kbd "C-c b") 'insert-banner-comment)
  (local-set-key (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-hook 'customize-cc-mode)
(add-hook 'c++-mode-hook 'customize-cc-mode)

;; -- Company Mode --

(defun customize-company-mode ()
  "Custom hook for `company-mode'."
  (local-set-key (kbd "C-<tab>") 'company-manual-begin)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(with-eval-after-load "company"
  (add-hook 'company-mode-hook 'customize-company-mode)
  ;; Add irony as a backend for company
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)
  ;; Enable Company mode in specific modes
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode))

;; -- Customization File --

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; -- Display Time Mode --

;; Show a clock in the mode bar
(setq display-time-interval 5)
(setq display-time-24hr-format t)
(display-time-mode 1)

;; -- Elpy --

;; Enable Elpy
(with-eval-after-load "elpy"
  (elpy-enable))

;; -- Enable Functions --

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; -- ERC --

(setq erc-nick "xchrishawk")
(setq erc-user-full-name "Chris Vig")

;; -- Flycheck Mode --

(defun customize-flycheck-mode ()
  "Custom hook for `flycheck-mode'."
  (flycheck-irony-setup))

(with-eval-after-load "flycheck"
  (add-hook 'flycheck-mode-hook 'customize-flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))

;; -- Formatting --

;; Default auto-fill-mode fill-column to 80 instead of 72
(setq-default fill-column 80)

;; Never use tabs
(setq-default indent-tabs-mode nil)

;; -- Global Hooks --

;; Run `initialize' and `server-start' on startup
(add-hook 'after-init-hook 'initialize)
(add-hook 'after-init-hook 'server-start)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- IDO Mode --

;; Enable IDO mode
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

;; -- Irony Mode --

(defun customize-irony-mode()
  "Custom hook for `irony-mode'."
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)
  (irony-cdb-autosetup-compile-options))

(with-eval-after-load "irony"
  (add-hook 'irony-mode-hook 'customize-irony-mode)
  ;; Enable irony mode in specific modes
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode))

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

;; Additional keyboard commands
(global-set-key (kbd "C-c C-c") 'compile)

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

;; -- Python --

;; Use Python 3 interpreter
(with-eval-after-load "elpy"
  (setq python-shell-interpreter "python3"))

;; -- Racket --

(defun customize-racket-mode ()
  "Custom hook for `racket-mode'."
  (local-set-key (kbd "C-c l") 'insert-lambda))

(add-hook 'racket-mode-hook 'customize-racket-mode)
(add-hook 'racket-repl-mode-hook 'customize-racket-mode)

; Custom indentations
(put 'ifmap 'racket-indent-function 1)
(put 'place 'racket-indent-function 1)

;; -- SGML Mode --

;; Use 4-character indent for HTML, but guess the indent when opening a file
(setq sgml-basic-offset 4)
(add-hook 'sgml-mode-hook 'sgml-guess-indent)

;; -- User Info --

(setq user-full-name "Chris Vig")
(setq user-mail-address "chris@invictus.so")

;; -- Environment Variables --

;; Set $GIT_EDITOR to open Git buffers in Emacs, falling back to nano if needed
(setenv "GIT_EDITOR" "emacsclient -a 'nano'")

;; -- Miscellaneous --

;; Don't prompt for confirmation when quitting with running processes
(setq confirm-kill-processes nil)

;; -- MacOS-Specific Setup --

(when (eq system-type 'darwin)
  ;; Set font
  (set-frame-font "Menlo-15")
  ;; Swap default command and option keybindings so my pinkie doesn't fall off
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ring-bell-function 'ignore)
  ;; Disable annoying warning for Python
  (setq python-shell-completion-native-enable nil))

;; -- Computer-Specific Setup --

;; Starfighter (Chris's personal Macbook)
(when (string= (system-name) "starfighter")
  ;; Larger font
  (set-frame-font "Menlo-20")
  ;; Use IPython
  (with-eval-after-load "elpy"
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "-i --simple-prompt"))
  ;; Add python bin to path
  (add-to-path "/Users/chris/Library/Python/3.9/bin"))
