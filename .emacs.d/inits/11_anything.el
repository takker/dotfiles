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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything関連の設定
;;* ホームポジションのアルファベットで候補を選択できるようにする
(setq anything-enable-shortcuts 'alphabet)

;;; anything関連のキーバインド設定
;;* anything-mapの設定
;;** C-SPCは日本語入力ON/OFFなので、C-@でマークをトグル
(define-key anything-map (kbd "C-@") 'anything-toggle-visible-mark)

;;* anythingコマンドのprefixを C-x C-a に設定
(define-prefix-command 'my-anything-map)
(global-set-key [(C x)(C a)] 'my-anything-map)

;;** C-o => moccur
(define-key my-anything-map (kbd "C-o") 'anything-c-moccur-occur-by-moccur)
;;** C-s => isearch-forward
(define-key my-anything-map (kbd "C-s") 'anything-c-moccur-isearch-forward)
;;** C-r => isearch-backward
(define-key my-anything-map (kbd "C-r") 'anything-c-moccur-isearch-backward)
;;** C-f => anything-for-files
(define-key my-anything-map (kbd "C-f") 'anything-for-files)
;;** C-b => anything-buffers+
(define-key my-anything-map (kbd "C-b") 'anything-buffers+)
;;** g   => anything-grep
(define-key my-anything-map (kbd "g") 'anything-grep)
;;** C-/ => anything-resume
(define-key my-anything-map (kbd "C-/") 'anything-resume)
;;** m   => anything-all-mark-rings
(define-key my-anything-map (kbd "m") 'anything-all-mark-rings)
;;** a   => anything-apropos
(define-key my-anything-map (kbd "a") 'anything-apropos)
;;** C-y => anything-show-kill-ring
(define-key my-anything-map (kbd "C-y") 'anything-show-kill-ring)
