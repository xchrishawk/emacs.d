;; functions.el
;; Chris Vig (chris@invictus.so)

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

;; -- Window Management --

(defun prev-window (count)
  "Opposite of other-window."
  (interactive "p")
  (other-window (- count)))

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

(defun insert-lambda-char ()
  "Inserts a lambda character (λ) at point."
  (interactive)
  (insert "λ"))

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
