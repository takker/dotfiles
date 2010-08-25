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

;;* ruby-electric.el --- electric editing commands for ruby files
(require 'ruby-electric)

;;* ruby-block.el
(require 'ruby-block)
(ruby-block-mode t)
;; ハイライトの方法
;; 'nothing: なにもしない
;; 'minibuffer:ミニバッファ
;; 'overlay: オーバレイ
(setq ruby-block-highlight-toggle t)    ; ミニバッファ+オーバレイ

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

;;* ruby用flymake
;; http://d.hatena.ne.jp/khiker/20070630/emacs_ruby_flymake
(require 'flymake)
;;** フェイスの設定
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;;* ruby-mode-hook
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)

  ;;** インデントは空白2文字
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)

  ;;** Don't want flymake mode for ruby regions in rhtml files
  (if (not (null buffer-file-name)) (flymake-mode))

  ;;** RSenseの設定
  ;; (find-file "/home/taka/usr/opt/rsense-0.3/doc/manual.ja.txt")
  ;;*** C-c C-/ => メソッド名などの補完
  (local-set-key [(C c) (C /)] 'ac-complete-rsense)
  (local-set-key [(C c) (C t)] 'rsense-type-help)
  (local-set-key [(C c) (C j)] 'rsense-jump-to-definition)
  (local-set-key [(C c) (C w)] 'rsense-where-is)
  ;;*** '.'や'::'の入力後に自動的に補完
  ;; (add-to-list 'ac-sources 'ac-source-rsense-method)
  ;; (add-to-list 'ac-sources 'ac-source-rsense-constant)
  ;;** キーバインドの追加
  ;; -------------------------
  ;;** C-m         newline-and-indent
  ;;** C-j         行末に移動+C-m
  ;;** C-c C-d     デバッガの起動
  ;;** C-c C-c     Compile
  (local-set-key [(C m)] 'ruby-reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") (kbd "C-e C-m"))
  (local-set-key [(C c) (C c)] 'compile)
  (local-set-key [(C c) (C d)] 'rubydb)
  (local-set-key [(C c) (C a)] 'ruby-beginning-of-defun)
  ;; (local-set-key [(C c) (C b)] 'ruby-backward-sexp)
  ;; (local-set-key [(C c) (C d)] 'ruby-insert-end)
  ;; (local-set-key [(C c) (C e)] 'ruby-end-of-defun)
  (local-set-key [(C c) (C f)] 'ruby-forward-sexp)
  (local-set-key [(C c) (C n)] 'ruby-end-of-block)
  (local-set-key [(C c) (C p)] 'ruby-beginning-of-block)
  ;; (local-set-key [(C c) (C q)] 'ruby-indent-exp)
  (local-unset-key "{")                 ; for smartchr
  )
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
