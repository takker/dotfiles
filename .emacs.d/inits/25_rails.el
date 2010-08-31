;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; rails用の設定
;; ;;* emacs-rails
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;; (setq rails-use-mongrel t)
;; (require 'rails)
;; ;;* C-c C-. => 対応するファイルへの切り替え
;; ;;* C-c C-, => 行き先を選べるファイル切り替え
;; (define-key rails-minor-mode-map [(C c) (C ,)] 'rails-lib:run-primary-switch)
;; (define-key rails-minor-mode-map [(C c) (C .)] 'rails-lib:run-secondary-switch)

;;* Interactively Do Things
(require 'ido)
(ido-mode t)
;;* Rinari
(require 'rinari)
;;* rhtml-mode: .rhtmlや.html.erb用マイナーモード
(require 'rhtml-mode)
(add-hook-fn 'rhtml-mode-hook (rinari-launch))
