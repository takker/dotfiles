;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
(require 'org)
;;* 1階層下・上の見出し・項目を入力する
;; 参考:Emacsテクニックバイブル p.275
(defun org-insert-upheading (arg)
  "1レベル上の見出しを入力する"
  (interactive "P")
  (org-insert-heading arg)
  (cond ((org-on-heading-p) (org-do-promote))
        ((org-at-item-p) (org-indent-item -1))))
(defun org-insert-heading-dwim (arg)
  "現在と同じレベルの見出しを入力する。
C-uを付けると1レベル上、C-u C-uを付けると1レベル下の見出しを入力する。"
  (interactive "p")
  (case arg
    (4  (org-insert-subheading nil))     ; C-u
    (16 (org-insert-upheading nil))      ; C-u C-u
    (t  (org-insert-heading nil))))
(define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-dwim)

;;* 瞬時にメモを取る
;; 参考:Emacsテクニックバイブル p.277
(org-remember-insinuate)
;;** メモを格納するorgファイルの設定
(setq org-directory "~/.emacs.d/etc/memo"
      org-default-notes-file (expand-file-name "memo.org" org-directory))
;;** テンプレートの設定
(setq org-remember-templates
      '(("Note" ?n "** %?\n   %i\n   %a\n   %t" nil "inbox")
        ("ToDo" ?t "** TODO %?\n   %i\n   %a^n   %t" nil "inbox")))

;;* ToDoリストを作成する
(setq org-use-fast-todo-selection t
      org-todo-keywords
      '((sequence "ToDo(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
        (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")))

;;* C-c l => ハイパーリンクを作成する
(global-set-key (kbd "C-c l") 'org-store-link)
