;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;; mysqlを実行できるようexec-pathに追加しておく
(add-to-list 'exec-path
             (substring (shell-command-to-string "dirname $(which mysql)") 0 -1))

;;; SQLiの設定
;;* セミコロンをタイプするとクエリを送信する
(setq sql-electric-stuff 'semicolon)
