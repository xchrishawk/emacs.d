;;
;; make.el
;; Chris Vig (chris@invictus.so)
;;

;; -- Provide --

(provide 'make)

;; -- Public Variables --

(defvar make-build-dir nil
  "The working directory in which the `make' command will build the currently
active project. The `default-directory' variable will be temporarily set to this
value prior to executing `compile'.")

(defvar make-build-additional-args nil
  "Additional arguments which will be passed to make when the `make' command is
invoked, in addition to the target name.")

;; -- Private Variables --

(defvar make--history nil
  "History list for the `make' command.")

;; -- Functions --

(defun make (target)
  "Runs make to build `TARGET' in `make-build-dir'. If `make-build-additional-args' is
not `nil', it will be appended to the command."
  (interactive
   (progn
     (or make-build-dir (user-error "make-build-dir is not set!"))
     (list (read-string "Target: " nil 'make--history))))
  (let ((default-directory make-build-dir)
        (command (mapconcat 'identity (list "make" target make-build-additional-args) " ")))
    (compile command)))

(defun make-set-build-dir (dir)
  "Sets the value of `make-build-dir' to `DIR'."
  (interactive "DBuild directory: ")
  (setq make-build-dir dir))

(defun make-set-build-additional-args (args)
  "Sets the value of `make-build-additional-args' to `ARGS'."
  (interactive "sMake arguments: ")
  (setq make-build-additional-args args))
