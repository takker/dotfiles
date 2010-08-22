;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* python-mode時のhook関数
(defun python-mode-hooks ()
  (setq indent-tabs-mode nil
        py-indent-offset 4))

;;* hook追加
(add-hook 'python-mode-hook 'python-mode-hooks)

;;* flymakeの設定
;; http://tech.lampetty.net/tech/index.php/archives/380
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(defconst flymake-allowed-python-file-name-masks
  '(("\\.py$" flymake-python-init)))
(defvar flymake-python-err-line-patterns
  '(("\\(.*\\):\\([0-9]+\\):\\(.*\\)" 1 2 nil 3)))

(defun flymake-python-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-python-file-name-masks))
  (setq flymake-err-line-patterns flymake-python-err-line-patterns)
  (flymake-mode t))
(add-hook 'python-mode-hook '(lambda () (flymake-python-load)))
