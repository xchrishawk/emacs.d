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

(defvar make-run-dir nil
  "The working directory to execute `make-run-target' in when `make-run' is
invoked. If set to `nil', `default-directory' will be used.")

(defvar make-run-target nil
  "The command to run when `make-run' is invoked. Must be set before `make-run'
can be invoked.")

(defvar make-test-dir nil
  "The working directory to execute `make-run-test' in when `make-test' is
invoked. If set to `nil', `default-directory' will be used.")

(defvar make-test-target nil
  "The command to run when `make-test' is invoked.")

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

(defun make-run ()
  "Runs `make-run-target' in `make-run-dir'."
  (interactive)
  (or make-run-target (user-error "make-run-target is not set!"))
  (let ((default-directory (or make-run-dir default-directory)))
    (compile make-run-target)))

(defun make-test ()
  "Runs `make-test-target' in `make-test-dir'."
  (interactive)
  (or make-test-target (user-error "make-test-target is not set!"))
  (let ((default-directory (or make-test-dir default-directory)))
    (compile make-test-target)))

(defun make-set-build-dir (dir)
  "Sets the value of `make-build-dir' to `DIR'."
  (interactive "DBuild directory: ")
  (setq make-build-dir dir))

(defun make-set-build-additional-args (args)
  "Sets the value of `make-build-additional-args' to `ARGS'."
  (interactive "sMake arguments: ")
  (setq make-build-additional-args args))

(defun make-set-run-target (target)
  "Sets the value of `make-run-target' to `TARGET'."
  (interactive "FRun target: ")
  (setq make-run-target target))

(defun make-set-run-dir (dir)
  "Sets the value of `make-run-dir' to `DIR'."
  (interactive "DRun directory: ")
  (setq make-run-dir dir))

(defun make-set-test-target (target)
  "Sets the value of `make-test-target' to `TARGET'."
  (interactive "FTest target: ")
  (setq make-test-target target))

(defun make-set-test-dir (dir)
  "Sets the value of `make-test-dir' to `DIR'."
  (interactive "DTest directory: ")
  (setq make-test-dir dir))
