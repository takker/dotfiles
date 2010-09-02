;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
(global-set-key [(M g)] 'goto-line)
;; (global-set-key [(M h)] 'backward-kill-word) ; mark-paragraph
(global-set-key [(M k)] 'kill-whole-line) ; kill-sentence
(global-set-key [(C m)] 'newline-and-indent)
; (global-set-key [(C t)] 'dmacro-exec)
(global-set-key [(M /)] 'hippie-expand)   ;補完を強力に
(global-set-key [(C >)] 'end-of-buffer)
(global-set-key [(C <)] 'beginning-of-buffer)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; (global-set-key "\C-\\" 'ibus-toggle)   ; C-\ でmozcのトグル

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ctl-x-map
;;* C-x C-b => anything-buffers+
(define-key ctl-x-map [(C b)] 'anything-buffers+)

;;* C-x C-o => 次のウィンドウに移動
;;* C-x C-p => 前のウィンドウに移動
(define-key ctl-x-map [(C o)] 'other-window)
(global-set-key-fn [(C x) (C p)] nil  ;mark-page
                   (interactive)
                   (other-window -1))
;;* C-x o => 連続する空行の削除
(define-key ctl-x-map [(o)] 'delete-blank-lines)

;;* C-x C-d => 別のウィンドウでdired
(define-key ctl-x-map [(C d)] 'ffap-dired-other-window)

;;* C-x C-r => 最近使ったファイルを表示
(define-key ctl-x-map [(C r)] 'recentf-open-files)

;;* C-x C-: => 辞書から英単語を補完
;;* C-x C-] => ispellを使ったスペルチェック
(define-key ctl-x-map [(C :)] 'ispell-complete-word)
(define-key ctl-x-map [(C \])] 'ispell-word)

;;* C-x C-i => リージョンのインデント
(define-key ctl-x-map [(C i)] 'indent-region)

;;* C-x C-h => help
(define-key ctl-x-map [(C h)] 'help-command)

;;* C-x C-l => バッファ一覧
(define-key ctl-x-map [(C l)] 'list-buffers) ; downcase-region

;;* C-x C-@ => 次のエラー位置へジャンプ
(define-key ctl-x-map [(C @)] 'next-error)

;;* C-x C-e: eval系のprefixキーにする
(define-prefix-command 'my-eval-map)
(global-set-key [(C x) (C e)] 'my-eval-map)
;;** C-: => eval-expression
;;** C-b => eval-buffer
;;** C-d => eval-defun
;;** C-e => eval-last-sexp
;;** C-r => eval-region
(define-key my-eval-map [(C :)] 'eval-expression)
(define-key my-eval-map [(C b)] 'eval-buffer)
(define-key my-eval-map [(C d)] 'eval-defun)
(define-key my-eval-map [(C e)] 'eval-last-sexp)
(define-key my-eval-map [(C r)] 'eval-region)

;;* C-x C-t: thing系のprefixキー
(define-prefix-command 'my-thing-map)
(global-set-key [(C x) (C t)] 'my-thing-map)
;;** C-b => mark-symbol
;;** C-c => mark-comment
;;** C-d => mark-defun*
;;** C-e => mark-sentence
;;** C-l => mark-up-list
;;** C-p => mark-paragraph*
;;** C-s => mark-string
;;** C-u => mark-url
;;** C-w => mark-word*
;;** C-x => mark-sexp
(define-key my-thing-map [(C b)] 'mark-symbol)
(define-key my-thing-map [(C c)] 'mark-comment)
(define-key my-thing-map [(C d)] 'mark-defun*)
(define-key my-thing-map [(C e)] 'mark-sentence)
(define-key my-thing-map [(C l)] 'mark-up-list)
(define-key my-thing-map [(C p)] 'mark-paragraph*)
(define-key my-thing-map [(C s)] 'mark-string)
(define-key my-thing-map [(C u)] 'mark-url)
(define-key my-thing-map [(C w)] 'mark-word*)
(define-key my-thing-map [(C x)] 'mark-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 特殊キー
;;* <F8>をauto-install用のキーに割り当て
;;** w => from emacswiki
;;** u => from url
;;** g => from gist
;;** b => batch
(global-set-key (kbd "<f8> w") 'auto-install-from-emacswiki)
(global-set-key (kbd "<f8> u") 'auto-install-from-url)
(global-set-key (kbd "<f8> g") 'auto-install-from-gist)
(global-set-key (kbd "<f8> b") 'auto-install-batch)
