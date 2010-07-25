;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; anythingの設定全般
;;* anything-startup: anythingの初期設定
(require 'anything-startup)

;;* anything-grep: anythingでgrep
(require 'anything-grep)
;;** デフォルトのgrep-commandをackにする
;; (ackのない環境ではack.elを使う？ http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=ack.el )
(grep-apply-setting 'grep-command "ack-grep --color --nogroup ")

;;* anything-c-moccur: anythingでmoccur
;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
(require 'anything-c-moccur)
;;** スペース区切りで検索可能に
(setq moccur-split-word t)
;;** migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ; 第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

(setq anything-c-moccur-enable-initial-pattern t ; カーソル位置の語を最初のパターンに
      anything-c-moccur-enable-auto-look-flag t)  ;; 現在選択中の候補の位置を別のbufferに
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;;* descbinds-anything.el: キーバインドをanythingで表示
(require 'descbinds-anything)
(descbinds-anything-install)

;;; anything関連のキーバインド設定
;;* M-o   => moccur
;;* C-M-s => isearch-forward
;;* C-M-r => isearch-backward
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "C-M-s") 'anything-c-moccur-isearch-forward)
(global-set-key (kbd "C-M-r") 'anything-c-moccur-isearch-backward)

(global-set-key [(C x)(C f)] 'anything-for-files)
(global-set-key [(C x)(C b)] 'anything-buffers+)
