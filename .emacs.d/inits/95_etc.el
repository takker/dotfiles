;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* レジスタに以下のファイル・文字列を保存
(set-register ?e '(file . "~/.emacs.d/.emacs.el"))
(set-register ?h ";;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-")

;;* シーケンシャルコマンドの設定(sequential-command.el)
(define-sequential-command seq-home
  back-to-indentation beginning-of-line beginning-of-buffer seq-return)

;;* 同時押しで実行するコマンドの設定(key-chord.el)
(key-chord-define-global "jk" 'view-mode)
(key-chord-define-global "df" 'descbinds-anything)
(key-chord-define-global "fs" 'move-to-char-forward)
(key-chord-define-global "FS" 'move-to-char-backward)

;;* テンプレートの挿入
(setq auto-insert-alist
      (append
       '(
         ("_test\\.c" . "template_test.c")
         ("\\.c$" . "_.c")
         ("\\.h$" . "_.h")
         ("Main\\.java$" . "Main.java")
         ("\\.pl$" . "_.pl")
         ("\\.tex$" . "_.tex")
         ("Makefile$" . "_.Makefile")
         ("tc.+\\.rb$" . "tc__.rb")
         ("\\.rb$" . "_.rb")
         ("\\.xul$" . "_.xul")
         )
       auto-insert-alist))

;;* smartchrの設定
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
;; (global-set-key (kbd "=") (smartchr '("= " "== " "=== ")))
(global-set-key (kbd "=") (smartchr '("= " "=\"`!!'\"")))
(global-set-key (kbd ">") (smartchr '(">" "=> ")))
