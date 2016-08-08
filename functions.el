;; functions.el
;; Chris Vig (chris@invictus.so)

;; ---- Initialization ----

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

;; ---- Appearance ----

(defun set-font-size (size)
  "Sets the default font size."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height (* size 10)))

;; ---- Buffer Management ----

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

;; ---- File Management ----

(defun rename-current-buffer-file (new-file-name)
  "Renames the file associated with the current buffer."
  (interactive
   (if (not (buffer-file-name (current-buffer)))
       (error "%s is not a file buffer" (buffer-name (current-buffer)))
     (list (read-file-name "Rename to: "))))
  (let ((original-file-name (buffer-file-name (current-buffer))))
    (write-file new-file-name t)
    (delete-file original-file-name)))

;; ---- Window Management ----

(defun prev-window (count)
  "Opposite of other-window."
  (interactive "p")
  (other-window (- count)))

;; ---- Misc ----

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
