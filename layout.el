;; layout.el
;; Chris Vig (chris@invictus.so)

(defun initial-layout ()
  "Resets the frame to the default layout."
  (interactive)

  (delete-other-windows)
  (switch-to-buffer (buffer-special-name "scratch"))
  
  (split-window-horizontally)
  (next-multiframe-window)
  (terminal))

(defun terminal ()
  "Opens an ansi-terminal buffer named \"term\", or, if the buffer already exists, select it."
  (interactive)

  (let* ((terminal-name "terminal")
	 (terminal-buffer-name (buffer-special-name terminal-name))
	 (bash-path "/bin/bash"))

    (if (buffer-exists terminal-buffer-name)
	(switch-to-buffer terminal-buffer-name)
      (ansi-term bash-path terminal-name))))

(defun buffer-exists (name)
  "Returns t if a buffer exists with the specified name."
  (not (eq nil (get-buffer name))))

(defun buffer-special-name (name)
  "Builds a \"special\" name for a buffer (e.g., \"*buffer*\" for buffer)."
  (concat (string ?*) name (string ?*)))
