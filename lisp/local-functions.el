;; local-functions.el
;; Chris Vig (chris@invictus.so)

;; -- Provides --

(provide 'local-functions)

;; -- Requires --

(require 'subr-x)

;; -- Initialization --

(defun initialize ()
  "Resets the frame to its initial layout."
  (interactive)
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

;; -- Appearance --

(defun set-font-size (size)
  "Sets the default font size."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height (* size 10)))

;; -- Module Management --

(defmacro if-feature-loaded (module then-form else-form)
  "Executes THEN-FORM if and only if MODULE is already loaded, otherwise executes
ELSE-FORM."
  (declare (indent 2))
  `(if (featurep ,module)
       ,then-form
     ,else-form))

(defmacro when-feature-loaded (module &rest body)
  "Executes BODY if and only if MODULE is already loaded."
  (declare (indent defun))
  `(when (featurep ,module)
     ,@body))

;; -- Buffer Management --

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
	 (bash-path "/bin/bash"))
    (if (buffer-exists terminal-buffer-name)
	(switch-to-buffer terminal-buffer-name)
      (ansi-term bash-path terminal-name))))

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

;; -- Template Projects --

(defun template-project (template-name project-name project-dir)
  "Builds a template project using TEMPLATE-NAME. Project name is set to
PROJECT-NAME, and project is placed in PROJECT-DIR."
  (interactive "sTemplate name: \nsProject name: \nGProject directory: ")
  (let* ((template-dir (concat (file-name-as-directory (expand-file-name "~/Templates")) template-name)))
    ;; Recursively copy entire directory structure
    (copy-directory template-dir project-dir nil nil t)
    ;; Fix up directory names
    (run-on-directories
     project-dir
     (lambda (dir)
       (when (string-match "\\(.+\\)\$\{TEMPLATE\}\\(/\\)?" dir)
	 (rename-file dir (concat (file-name-as-directory (match-string 1 dir)) project-name))))
     t)
    ;; Fix up file contents
    (run-on-files
     project-dir
     (lambda (file)
       (when (not (string-match "/\\.git/" file))
	 (with-temp-file file
	   (insert-file-contents file)
	   (goto-char (point-min))
	   (while (re-search-forward "\$\{TEMPLATE\}" nil t)
	     (replace-match project-name t)))))
     t)
    ;; Switch to project directory
    (cd project-dir)))

;; -- Window Management --

(defun prev-window (count)
  "Opposite of other-window."
  (interactive "p")
  (other-window (- count)))

;; -- Java Mode --

(defun ant (build-xml-directory target)
  "Searches up the directory path for a file named \"build.xml\". If found, prompts
for an Ant target to run, and then runs Ant in a compilation buffer."
  (interactive
   (let ((build-xml-dir (locate-dominating-file (buffer-file-name (current-buffer)) "build.xml")))
     (if (not build-xml-dir) (error "No build.xml found!"))
     (list build-xml-dir (read-string "Ant target: "))))
  (let* ((build-xml-path (expand-file-name "build.xml" build-xml-directory))
	 (command (mapconcat 'identity (list "ant" target "-f" build-xml-path) " ")))
    (compile command)))

;; -- Racket Mode --

(defun racket-move-contracts-from-define-to-provide ()
  "Moves all function contracts from (define/contract ...) forms to the recommended
(provide (contract-out ...)) form. The new (provide (contract-out ...)) form will
be inserted at the current `point'."
  (interactive)
  (let ((contract-list nil))
    (save-excursion
      ;; Loop through all define/contracts in buffer
      (goto-char (point-min))
      (while (re-search-forward "(define/contract" nil t)
	;; If we're in a comment or string, ignore this match
	(unless (nth 8 (syntax-ppss))
	  ;; Replace define/contract with define
	  (replace-match "(define")
	  (let (function-name contract)
	    ;; Get function name
	    (save-excursion
	      (re-search-forward "(\\([A-Za-z\-]+\\)")
	      (setq function-name (match-string 1)))
	    ;; Extract contract from buffer
	    (save-excursion
	      (forward-sexp)
	      (let ((marker (point)))
		(forward-sexp)
		(setq contract (string-trim (delete-and-extract-region marker (point))))))
	    ;; Add function name and contract to list
	    (push (cons function-name contract) contract-list)))))
    ;; Insert the new (provide (contract-out ...)) block
    (let ((marker (point)))
      (insert "(provide\n(contract-out")
      (dolist (item contract-list)
	(insert (format "\n[%s %s]" (car item) (cdr item))))
      (insert "))")
      (indent-region marker (point)))))

;; -- Misc --

(defun insert-header-comment ()
  "Inserts a mode-appropriate header comment at the beginning of the buffer,
unless the buffer already begins with a header comment."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((comment nil))
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (setq comment (format ";; %s\n;; %s (%s)\n\n"
			    (file-name-nondirectory (buffer-file-name (current-buffer)))
			    (user-full-name)
			    user-mail-address)))
     ((eq major-mode 'java-mode)
      (setq comment (format "/**\n * %s\n * @author %s (%s)\n */\n\n"
			    (file-name-nondirectory (buffer-file-name (current-buffer)))
			    (user-full-name)
			    user-mail-address))))
    (or comment (error "No format defined for this mode"))
    (save-excursion
      (goto-char (point-min))
      (unless (string-prefix-p comment (buffer-string))
	(insert comment)))))

(defun insert-lambda-char ()
  "Inserts a lambda character (λ) at point."
  (interactive)
  (insert "λ"))

(defun insert-left-arrow-char ()
  "Inserts a left arrow character (←) at point."
  (interactive)
  (insert "←"))

(defun fill-to-column (column char)
  "Fills the current line with CHAR up to column COLUMN. Sets mark at current point."
  (interactive "NFill to column: \ncCharacter:")
  (let* ((current-column (- (point) (line-beginning-position)))
	 (chars-required (- column current-column)))
    (push-mark)
    (insert-char char chars-required)))

(defun unindent-buffer ()
  "Removes any leading whitespace before all lines in this buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dotimes (line (line-number-at-pos (point-max)))
      (beginning-of-line)
      (fixup-whitespace)
      (forward-line))))
