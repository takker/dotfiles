;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; php-mode関連の設定
(autoload 'php-mode "php-mode")

;;;; mmm-mode
;; http://d.hatena.ne.jp/jimo1001/20071111/1194770814
;; (require 'mmm-mode)
;; (require 'mmm-sample)
(require 'mmm-auto)
(setq mmm-submode-decoration-level 2)
(invert-face 'mmm-default-submode-face t)
(setq mmm-font-lock-available-p t)
(setq mmm-global-mode 'maybe)
; (set-face-bold-p 'mmm-default-submode-face t)
(set-face-background 'mmm-default-submode-face nil)
(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . html-mode))
(defun save-mmm-c-locals ()
  (with-temp-buffer
    (php-mode)
    (dolist (v (buffer-local-variables))
      (when (string-match "\\`c-" (symbol-name (car v)))
        (add-to-list 'mmm-save-local-variables `(,(car v) nil, mmm-c-derived-modes))))))
(save-mmm-c-locals)
