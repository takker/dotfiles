;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; ruby-mode の設定
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(
                ("\\.rb$" . ruby-mode)
                ("[Rr]akefile$" . ruby-mode)
                ("\\.rjs$" . ruby-mode)
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

;;* ECB
(load-file "/home/taka/.emacs.d/elisp/cedet-1.0pre7/common/cedet.el")
(setq semantic-load-turn-useful-things-on t)
(require 'ecb)
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)
(defun ecb-toggle ()
  (interactive)
  (if ecb-minor-mode
      (ecb-deactivate)
    (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)

;;* rcodetools
(require 'rcodetools)
(setq rct-find-tag-if-available nil)

(require 'anything-rcodetools)
(setq rct-get-all-methods-command "PAGER=cat fri -l")
;; See docs
(define-key anything-map [(control ?;)] 'anything-execute-persistent-action)

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

;;* rsense: Rubyのための開発援助ツール
;; http://cx4a.org/software/rsense/index.ja.html
(setq rsense-home "/home/taka/usr/opt/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

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
  (local-set-key [(C c) (C /)] 'ac-complete-rsense)
  (local-set-key [(C c) (C t)] 'rsense-type-help)
  (local-set-key [(C c) (C j)] 'rsense-jump-to-definition)
  (local-set-key [(C c) (C w)] 'rsense-where-is)
  ;;*** '.'や'::'の入力後に自動的に補完
  ;; (add-to-list 'ac-sources 'ac-source-rsense-method)
  ;; (add-to-list 'ac-sources 'ac-source-rsense-constant)

  ;;** キーバインドの追加
  (local-set-key [(C m)] 'ruby-reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") (kbd "C-e C-m"))
  ;; (local-set-key [(C c) (C c)] 'compile)
  (local-set-key [(C c) (C d)] 'rubydb)
  (local-set-key [(C c) (C a)] 'ruby-beginning-of-defun)
  (local-set-key [(C c) (C e)] 'ruby-end-of-defun)
  (local-set-key [(C c) (e)] 'ruby-insert-end)
  (local-set-key [(C c) (C f)] 'ruby-forward-sexp)
  (local-set-key [(C c) (C b)] 'ruby-backward-sexp)
  (local-set-key [(C c) (C n)] 'ruby-end-of-block)
  (local-set-key [(C c) (C p)] 'ruby-beginning-of-block)
  (local-unset-key (kbd "TAB"))         ; yas/expand

  ;;** smartchr
  (setq smartchr-hash
        '(
          ">"
          "=> "
          "=> '`!!''"
          "=> \"`!!'\""
          )
        smartchr-regexp
        '(
          "~"
          "=~ /`!!'/"
          "!~ /`!!'/"
          )
        smartchr-block
        '(
          "{ `!!' }"
          "{ |`!!'|  }"
          "{ "
          )
        smartchr-comment
        '(
          "#"
          "#{`!!'}"
          "# => "
          ))
  (local-set-key (kbd ">") (smartchr smartchr-hash))
  (local-set-key (kbd "~") (smartchr smartchr-regexp))
  (local-unset-key (kbd "{"))           ; ruby-electric-curlies
  (local-set-key (kbd "{") (smartchr smartchr-block))
  (local-set-key (kbd "#") (smartchr smartchr-comment))

  ;; ;;** rcodetools用キーバインド
  ;; (local-set-key [(M C i)] 'rct-complete-symbol)
  (local-set-key [(C c) (C :)] 'ruby-toggle-buffer)
  (local-set-key [(C c) (C u)] 'xmp)
  (local-set-key [(C c) (C h)] 'rct-ri)
  )
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
