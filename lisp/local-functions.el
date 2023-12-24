;;
;; lisp/local-functions.el
;; Chris Vig (chris@invictus.so)
;;

;; -- Provides --

(provide 'local-functions)

;; -- Appearance --

(defun set-font-size (size)
  "Sets the default font size."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height (* size 10)))

(defun set-selected-frame-size (&optional width height)
  "Sets the active frame size to the specified width and height (in columns and
rows, respectively. If `width' and `height' are not specified, they default to
80 and 24, respectively."
  (interactive)
  (when (window-system)
    (set-frame-size
     (selected-frame)
     (or width 80)
     (or height 24))))

;; -- Buffers --

(defun buffer-exists (name)
  "Returns t if a buffer exists with the specified name."
  (not (eq nil (get-buffer name))))

(defun buffer-special-name (name)
  "Builds a non-file name for a buffer (e.g., \"*buffer*\" for \"buffer\")."
  (concat (string ?*) name (string ?*)))

(defun scratch ()
  "Opens a \"*scratch*\" buffer, or switches to it if it already exists."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun terminal ()
  "Opens an ansi-term buffer named \"terminal\", or selects it if it alredy exists."
  (interactive)
  (let* ((terminal-name "terminal")
	 (terminal-buffer-name (buffer-special-name terminal-name))
         (shell-path
          (if (eq system-type 'darwin)
              "/bin/zsh"
            "/bin/bash")))
    (if (buffer-exists terminal-buffer-name)
	(switch-to-buffer terminal-buffer-name)
      (ansi-term shell-path terminal-name))))

;; -- Editing --

(defun fill-to-column (column char)
  "Fills the current line with CHAR up to column COLUMN. Sets mark at current point."
  (interactive "NFill to column: \ncCharacter:")
  (let* ((current-column (- (point) (line-beginning-position)))
	 (chars-required (- column current-column)))
    (push-mark)
    (insert-char char chars-required)))

(defun insert-banner-comment ()
  "Inserts a C++-formatted banner comment."
  (interactive)
  (insert "/* -- ")
  (save-excursion
    (insert " -- */")))

(defun insert-header-comment ()
  "Inserts a mode-appropriate header comment at the beginning of the buffer,
unless the buffer already begins with a header comment."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (let ((comment nil))
      (cond
       ((or (eq major-mode 'emacs-lisp-mode)
            (eq major-mode 'racket-mode))
        (setq comment (format ";;\n;; %s\n;; %s (%s)\n;;\n\n"
                              (file-name-nondirectory (buffer-file-name (current-buffer)))
                              (user-full-name)
                              user-mail-address)))
       ((eq major-mode 'java-mode)
        (setq comment (format "/**\n * %s\n * @author %s (%s)\n */\n\n"
                              (file-name-nondirectory (buffer-file-name (current-buffer)))
                              (user-full-name)
                              user-mail-address)))
       ((or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
        (setq comment (format "/**\n * @file	%s\n * @author	%s (%s)\n * @date	%s\n */\n\n"
                              (file-name-nondirectory (buffer-file-name (current-buffer)))
                              (user-full-name)
                              user-mail-address
                              (format-time-string "%Y/%m/%d")))))
      (or comment (error "No format defined for this mode"))
      (unless (string-prefix-p comment (buffer-string))
        (goto-char (point-min))
        (insert comment)))))

(defun insert-lambda ()
  "Inserts a lambda character at point."
  (interactive)
  (insert "\u03BB"))

(defun unindent-buffer ()
  "Removes any leading whitespace before all lines in this buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dotimes (line (line-number-at-pos (point-max)))
      (beginning-of-line)
      (fixup-whitespace)
      (forward-line))))

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

;; -- Initialization --

(defun initialize ()
  "Resets the frame to its initial layout."
  (interactive)
  (cd "~")
  (delete-other-windows)
  (switch-to-buffer (buffer-special-name "scratch"))
  (split-window-horizontally)
  (next-multiframe-window)
  (terminal))

(defun reinitialize (confirm)
  "Kills all buffers, then resets the frame to its initial layout."
  (interactive
   (list (yes-or-no-p "Reinitialize? ")))
  (when confirm
    (mapc 'kill-buffer (buffer-list))
    (initialize)))

;; -- Minor Modes --

(define-minor-mode greek-input
  "Toggle Greek input mode."
  :init-value nil
  :lighter " Greek"
  :keymap '(((kbd "C-c g a") . "α")
            ((kbd "C-c g b") . "β")
            ((kbd "C-c g g") . "γ")
            ((kbd "C-c g d") . "δ")
            ((kbd "C-c g e") . "ε")
            ((kbd "C-c g z") . "ζ")
            ((kbd "C-c g h") . "η")
            ((kbd "C-c g q") . "θ")
            ((kbd "C-c g i") . "ι")
            ((kbd "C-c g k") . "κ")
            ((kbd "C-c g l") . "λ")
            ((kbd "C-c g m") . "μ")
            ((kbd "C-c g n") . "ν")
            ((kbd "C-c g x") . "ξ")
            ((kbd "C-c g o") . "ο")
            ((kbd "C-c g p") . "π")
            ((kbd "C-c g r") . "ρ")
            ((kbd "C-c g s") . "σ")
            ((kbd "C-c g t") . "τ")
            ((kbd "C-c g u") . "υ")
            ((kbd "C-c g f") . "ϕ")
            ((kbd "C-c g j") . "φ")
            ((kbd "C-c g c") . "χ")
            ((kbd "C-c g y") . "ψ")
            ((kbd "C-c g w") . "ω")
            ((kbd "C-c g A") . "Α")
            ((kbd "C-c g B") . "Β")
            ((kbd "C-c g G") . "Γ")
            ((kbd "C-c g D") . "Δ")
            ((kbd "C-c g E") . "Ε")
            ((kbd "C-c g Z") . "Ζ")
            ((kbd "C-c g H") . "Η")
            ((kbd "C-c g Q") . "Θ")
            ((kbd "C-c g I") . "Ι")
            ((kbd "C-c g K") . "Κ")
            ((kbd "C-c g L") . "Λ")
            ((kbd "C-c g M") . "Μ")
            ((kbd "C-c g N") . "Ν")
            ((kbd "C-c g X") . "Ξ")
            ((kbd "C-c g O") . "Ο")
            ((kbd "C-c g P") . "Π")
            ((kbd "C-c g R") . "Ρ")
            ((kbd "C-c g S") . "Σ")
            ((kbd "C-c g T") . "Τ")
            ((kbd "C-c g U") . "Υ")
            ((kbd "C-c g F") . "Φ")
            ((kbd "C-c g J") . "Φ")
            ((kbd "C-c g C") . "Χ")
            ((kbd "C-c g Y") . "Ψ")
            ((kbd "C-c g W") . "Ω")))

;; -- Path Management --

(defun add-to-path (path)
  "Adds the specified string to the PATH and exec-path."
  (push path exec-path)
  (setenv "PATH" (concat path ":" (getenv "PATH"))))

;; -- Templates --

(defun template-project (template-dir project-dir project-name)
  "Builds a template project from `TEMPLATE-DIR'. Project name is set to
`PROJECT-NAME', and project is placed in `PROJECT-DIR'."
  (interactive
   (let* ((template-dir (read-directory-name "Template directory: " "~/Templates" nil t))
          (project-dir (read-directory-name "Project directory: " "~/Developer"))
          (project-name (read-string "Project name: " (file-name-nondirectory project-dir))))
     (list template-dir project-dir project-name)))
  (let* ((template-dir (file-name-as-directory (expand-file-name template-dir)))
         (project-dir (file-name-as-directory (expand-file-name project-dir))))
    ;; Confirm
    (or (yes-or-no-p (concat "Copy template " template-dir " to " project-dir "? "))
        (user-error "Cancelled."))
    ;; Recursively copy entire directory structure
    (copy-directory template-dir project-dir nil nil t)
    ;; Fix up directory names
    (run-on-directories
     project-dir
     (lambda (dir)
       (when (string-match "\\(.+\\)\$\{TEMPLATE_PROJECT_NAME\}\\(/\\)?" dir)
	 (rename-file dir (concat (file-name-as-directory (match-string 1 dir)) project-name))))
     t)
    ;; Fix up file contents
    (run-on-files
     project-dir
     (lambda (file)
       (when (not (string-match "/\\.git/" file))
	 (with-temp-file file
	   (insert-file-contents file)
           (dolist (item (list (cons "\$\{TEMPLATE_PROJECT_NAME\}" project-name)
                               (cons "\$\{TEMPLATE_PROJECT_DIR\}" project-dir)))
             (let ((pattern (car item))
                   (replacement (cdr item)))
               (goto-char (point-min))
               (while (re-search-forward pattern nil t)
                 (replace-match replacement t)))))))
     t)
    ;; Run initialization script, if one exists
    (let ((init-script (concat project-dir "init.sh")))
      (when (file-exists-p init-script)
        (let ((default-directory project-dir))
          (compile init-script))))))

;; -- Window Management --

(defun prev-window (count)
  "Opposite of `other-window'."
  (interactive "p")
  (other-window (- count)))
