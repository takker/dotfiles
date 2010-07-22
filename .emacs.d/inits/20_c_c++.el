;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; C-mode,C++-modeの設定

;;* スタイルの指定
(defconst my-c-style
  '(
    ;;** 基本オフセット量の設定
    (c-basic-offset             . 4)
    ;;** tab キーでインデントを実行
    (c-tab-always-indent        . t)
    ;;** コメント行のオフセット量の設定
    (c-comment-only-line-offset . 0)
    ;;** カッコ前後の自動改行処理の設定
    (c-hanging-braces-alist
     . (
        (class-open before after)       ; クラス宣言の'{'の前後
        (class-close after)             ; クラス宣言の'}'の後
        (defun-open before after)       ; 関数宣言の'{'の前後
        (defun-close after)             ; 関数宣言の'}'の後
        (inline-open after)             ; クラス内のインライン
                                        ; 関数宣言の'{'の後
        (inline-close after)            ; クラス内のインライン
                                        ; 関数宣言の'}'の後
        (brace-list-open after)         ; 列挙型、配列宣言の'{'の後
        (brace-list-close before after) ; 列挙型、配列宣言の'}'の前後
        (block-open after)              ; ステートメントの'{'の後
        (block-close after)             ; ステートメントの'}'前後
        (substatement-open after)       ; サブステートメント
                                        ; (if 文等)の'{'の後
        (substatement-close)            ; サブステートメント
                                        ; (if 文等)の'}'
        (statement-case-open after)     ; case 文の'{'の後
        (extern-lang-open before after) ; 他言語へのリンケージ宣言の
                                        ; '{'の前後
        (extern-lang-close before)      ; 他言語へのリンケージ宣言の
                                        ; '}'の前
        ))
    ;;** コロン前後の自動改行処理の設定
    (c-hanging-colons-alist
     . (
        (case-label after)              ; case ラベルの':'の後
        (label after)                   ; ラベルの':'の後
        (access-label after)            ; アクセスラベル(public等)の':'の後
        (member-init-intro)             ; コンストラクタでのメンバー初期化
                                        ; リストの先頭の':'では改行しない
        (inher-intro before)            ; クラス宣言での継承リストの先頭の
                                        ; ':'では改行しない
        ))
    ;;** 挿入された余計な空白文字のキャンセル条件の設定
    ;;*** 下記の*を削除する
    (c-cleanup-list
     . (
        brace-else-brace                ; else の直前
                                        ; "} * else {"  ->  "} else {"
        brace-elseif-brace              ; else if の直前
                                        ; "} * else if (.*) {"
                                        ; ->  } "else if (.*) {"
        empty-defun-braces              ; 空のクラス・関数定義の'}' の直前
                                        ;；"{ * }"  ->  "{}"
        defun-close-semi                ; クラス・関数定義後の';' の直前
                                        ; "} * ;"  ->  "};"
        list-close-comma                ; 配列初期化時の'},'の直前
                                        ; "} * ,"  ->  "},"
        scope-operator                  ; スコープ演算子'::' の間
                                        ; ": * :"  ->  "::"
        ))
    ;;** オフセット量の設定
    ;; 必要部分のみ抜粋(他の設定に付いては info 参照)
    ;; オフセット量は下記で指定
    ;; +  c-basic-offsetの 1倍, ++ c-basic-offsetの 2倍
    ;; -  c-basic-offsetの-1倍, -- c-basic-offsetの-2倍
    (c-offsets-alist
     . (
        (arglist-intro          . ++)   ; 引数リストの開始行
        (arglist-close          . c-lineup-arglist) ; 引数リストの終了行
        (substatement-open      . 0)    ; サブステートメントの開始行
        (statement-cont         . ++)   ; ステートメントの継続行
        (case-label             . 0)    ; case 文のラベル行
        (label                  . 0)    ; ラベル行
        (block-open             . 0)    ; ブロックの開始行
        (member-init-intro      . ++)   ; メンバオブジェクトの初期化リスト
        ))
    ;; インデント時に構文解析情報を表示する
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")

;;* hook 用の関数の定義
; 参考: http://www.cozmixng.org/webdav/kensuke/site-lisp/mode/my-c.el
(defun my-c-mode-common-hook ()
  ;;** my-c-style を登録して有効にする
  (c-add-style "My C Programming Style" my-c-style t)

  ;; (c-set-style "k&r")
  ;; (c-set-style "gnu")
  ;; (c-set-style "bsd")
  ;; (c-set-style "linux")
  ;; (c-set-style "cc-mode")
  ;; (c-set-style "stroustrup")
  ;; (c-set-style "ellemtel")
  ;; (c-set-style "whitesmith")
  ;; (c-set-style "python")

  ;;** auto-fill-modeを有効にする
  (auto-fill-mode t)
  ;;** タブ長の設定
  (make-variable-buffer-local 'tab-width)
  (setq tab-width 4)
  ;;** タブの代わりにスペースを使う
  (setq indent-tabs-mode nil)
  ;;** 自動改行(auto-newline)を有効にする
  ;; 連続する空白の一括削除(hungry-delete)を有効にする
  (c-toggle-auto-hungry-state t)
  ;;** セミコロンで改行しない
  (setq c-hunging-semi&comma-criteria nil)

  ;; キーバインドの追加
  ;; -------------------------
  ;; C-c C-c    コンパイル
  ;; C-c C-d    gdbの起動
  ;; C-c C-r    gdbウィンドウを元に戻す
  ;; C-h        空白の一括削除
  (define-key c-mode-base-map [(C c) (C c)] 'compile)
  (define-key c-mode-base-map [(C c) (C d)] 'gdb)
  (define-key c-mode-base-map [(C c) (C r)] 'gdb-restore-windows)
  (define-key c-mode-base-map [(C h)] 'c-electric-backspace)

  ;;** コンパイルコマンドの設定
  ;; (setq compile-command "gcc ")
  (setq compile-command "make -k ")
  ;; (setq command-command "gmake -k ")

  ;;** 自動スペルチェック
  (flyspell-mode t)
  ;;** c-mode の outline
  (autoload 'c-outline "c-outline" nil t)
  )

;; モードに入るときに呼び出す hook の設定
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; move current function up
(defun my-move-function-up ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (kill-region (region-beginning) (region-end))
    (c-beginning-of-defun 1)
    (yank)))

;; move current function down
(defun my-move-function-down ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (kill-region (region-beginning) (region-end))
    (c-beginning-of-defun -1)
    (yank)))
(defun mark-c-scope-beg ()
  "Marks the c-scope (region between {}) enclosing the point.
   Naive, as will be confused by { } within strings"
  (let
	  ((scope-depth 1))
	(while (not (= scope-depth 0))
	  (search-backward-regexp "}\\|{")
	  (if (string= (char-to-string (char-before)) "}")
		  (setq scope-depth (1+ scope-depth))
			(setq scope-depth (1- scope-depth)))))
  (point))

(defun mark-c-scope-end ()
  "Marks the c-scope (region between {}) enclosing the point.
   Naive, as will be confused by { } within strings"
  (let
	  ((scope-depth 1))
	(while (not (= scope-depth 0))
	  (search-forward-regexp "}\\|{")
	  (if (string= (char-to-string (char-before)) "}")
		  (setq scope-depth (1- scope-depth))
			(setq scope-depth (1_ scope-depth)))))
  (point))

(defun kill-c-scope ()
  (interactive)
  (let
	  ((inital-point (point)))
	(save-excursion
	  (let
		((beg (mark-c-scope-beg)))
		(goto-char inital-point)
		(let ((end (mark-c-scope-end))))))))

