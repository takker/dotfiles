;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
(global-set-key [(M g)] 'goto-line)
(global-set-key [(M h)] 'backward-kill-word) ; mark-paragraph
(global-set-key [(M k)] 'kill-whole-line) ; kill-sentence
(global-set-key [(C m)] 'newline-and-indent)
; (global-set-key [(C t)] 'dmacro-exec)
(global-set-key [(M /)] 'hippie-expand)   ;補完を強力に
(global-set-key [(C >)] 'end-of-buffer)
(global-set-key [(C <)] 'beginning-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctl-x-map

;; C-x C-e: eval系のprefixキーにする
(define-prefix-command 'my-eval-map)
(global-set-key [(C x) (C e)] 'my-eval-map)
; C-: => eval-expression
; C-b => eval-buffer
; C-d => eval-defun
; C-e => eval-last-sexp
; C-r => eval-region
(define-key my-eval-map [(C :)] 'eval-expression)
(define-key my-eval-map [(C b)] 'eval-buffer)
(define-key my-eval-map [(C d)] 'eval-defun)
(define-key my-eval-map [(C e)] 'eval-last-sexp)
(define-key my-eval-map [(C r)] 'eval-region)

;; 最近使ったファイルを表示
(define-key ctl-x-map [(C r)] 'recentf-open-files)

;; C-x C-: => 辞書から英単語を補完
;; C-x C-] => ispellを使ったスペルチェック
(define-key ctl-x-map [(C :)] 'ispell-complete-word)
(define-key ctl-x-map [(C \])] 'ispell-word)

;; C-x C-i => リージョンのインデント
(define-key ctl-x-map [(C i)] 'indent-region)

;; C-x C-h => help
(define-key ctl-x-map [(C h)] 'help-command)

;; C-x C-b => バッファ切り替え
;; C-x C-l => バッファ一覧
; (define-key ctl-x-map [(C b)] 'iswitchb-buffer)
(define-key ctl-x-map [(C l)] 'list-buffers) ; downcase-region

;; C-x C-@ => 次のエラー位置へジャンプ
(define-key ctl-x-map [(C @)] 'next-error)

