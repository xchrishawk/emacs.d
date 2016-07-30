;; libs.el
;; Chris Vig (chris@invictus.so)

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             t)

;; Enable IDO mode
(require 'ido)
(ido-mode t)

;; Enable windmove
(windmove-default-keybindings)
