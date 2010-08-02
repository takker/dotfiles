;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; ruby-mode の設定
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(
                ("\\.rb$" . ruby-mode)
                ("[Rr]akefile$" . ruby-mode)
                )
              auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
;;* rubydb
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

;;* ruby-mode-hook
(add-hook-fn 'ruby-mode-hook
             (inf-ruby-keys)
             ;;** キーバインドの追加
             ;; -------------------------
             ;;** C-m         newline-and-indent
             ;;** C-j         行末に移動+C-m
             ;;** C-c C-d     デバッガの起動
             ;;** C-c C-c     Compile
             (define-key ruby-mode-map [(C m)] 'ruby-reindent-then-newline-and-indent)
             (define-key ruby-mode-map (kbd "C-j") (kbd "C-e C-m"))
             (define-key ruby-mode-map [(C c) (C c)] 'compile)
             (define-key ruby-mode-map [(C c) (C d)] 'rubydb)
             (define-key ruby-mode-map [(C c) (C a)] 'ruby-beginning-of-defun)
             ;; (define-key ruby-mode-map [(C c) (C b)] 'ruby-backward-sexp)
             ;; (define-key ruby-mode-map [(C c) (C d)] 'ruby-insert-end)
             ;; (define-key ruby-mode-map [(C c) (C e)] 'ruby-end-of-defun)
             (define-key ruby-mode-map [(C c) (C f)] 'ruby-forward-sexp)
             (define-key ruby-mode-map [(C c) (C n)] 'ruby-end-of-block)
             (define-key ruby-mode-map [(C c) (C p)] 'ruby-beginning-of-block)
             ;; (define-key ruby-mode-map [(C c) (C q)] 'ruby-indent-exp)
             )

;;* ruby-electric.el --- electric editing commands for ruby files
(require 'ruby-electric)
(add-hook-fn 'ruby-mode-hook (ruby-electric-mode t))

;;* ruby-block.el
(require 'ruby-block)
(ruby-block-mode t)
;; ハイライトの方法
;; 'nothing: なにもしない
;; 'minibuffer:ミニバッファ
;; 'overlay: オーバレイ
(setq ruby-block-highlight-toggle t)    ; ミニバッファ+オーバレイ

;;* インデントは空白2文字
(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)

;;* alignの設定
(require 'align)
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
