;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
(require 'flymake)
(global-set-key [(M e)] 'flymake-goto-next-error)
(global-set-key [(M E)] 'flymake-goto-prev-error)

;; gotoした際にエラーメッセージをminibufferに表示する
(defun display-error-message ()
  (message (get-char-property (point) 'help-echo)))
(defadvice flymake-goto-prev-error (after flymake-goto-prev-error-display-message)
  (display-error-message))
(defadvice flymake-goto-next-error (after flymake-goto-next-error-display-message)
  (display-error-message))
(ad-activate 'flymake-goto-prev-error 'flymake-goto-prev-error-display-message)
(ad-activate 'flymake-goto-next-error 'flymake-goto-next-error-display-message)

;;* C-c p => ツールチップにエラー内容を表示
(defun next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (popup-tip err))))
(define-key mode-specific-map [(p)] 'next-flymake-error)