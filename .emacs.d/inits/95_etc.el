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
(key-chord-define-global "kl" 'org-remember)

;;* hippie-expandの補完の優先順位
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;* smartchrの設定
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
(global-set-key (kbd "=") (smartchr '("= " "=\"`!!'\"")))
(global-set-key (kbd ">") (smartchr '(">" "=> ")))
