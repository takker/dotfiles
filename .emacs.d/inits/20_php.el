;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; php-mode関連の設定
(autoload 'php-mode "php-mode" t)
(defun php-mode-hooks ()
  (setq tab-width 2)
  (setq indent-tabs-mode nil))
(add-hook 'php-mode-hook 'php-mode-hooks)

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "gray15")
(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . yahtml-mode))
