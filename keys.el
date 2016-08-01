;; keys.el
;; Chris Vig (chris@invictus.so)

; Function to insert lambda character
(defun insert-lambda ()
  "Insert a lambda character (λ) at point."
  (interactive)
  (insert "λ"))

; Add insert-lambda to Racket mode
(defun custom-racket-mode ()
  "Modify keymaps used by racket-mode."
  (local-set-key (kbd "C-c l") 'insert-lambda))
(add-hook 'racket-mode-hook 'custom-racket-mode)
