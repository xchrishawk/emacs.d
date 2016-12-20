;; local-make.el
;; Chris Vig (chris@invictus.so)

;; -- Provides --

(provide 'local-make)

;; -- Variables --

(defvar make-working-directory nil
  "The current directory to run the `MAKE' command in.")

(defun make-set-working-directory (dir)
  "Sets the value of `MAKE-WORKING-DIRECTORY'."
  (interactive "DMakefile directory: ")
  (setq make-working-directory dir))

(defun make (target)
  "Runs make in `MAKE-WORKING-DIRECTORY'."
  (interactive
   (progn
     (if (not make-working-directory) (error "make-working-directory not set!"))
     (list (read-string "Target: "))))
  (let ((command (mapconcat 'identity (list "make" "-C" make-working-directory target) " ")))
    (compile command)))
