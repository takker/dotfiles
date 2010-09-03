;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; yasnippetやauto-insertなどのテンプレート関係の設定

;;; yasnippet
;;* yasunippet normal install
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/etc/snippets")
(require 'yasnippet-config)
(global-set-key (kbd "C-x C-y") 'yas/oneshot-snippet)

;;* auto-insert
(auto-insert-mode)
;;** テンプレートの保存ディレクトリ
(setq auto-insert-directory "~/.emacs.d/etc/templates/")

;;* テンプレートの挿入
(define-auto-insert "\\.h$" (lambda () (insert "once") (yas/expand)))
(setq auto-insert-alist
      (append
       '(
         ("\\.c$" . "c-template.c")
         ("\\.pl$" . "perl-template.pl")
         ("tc.+\\.rb$" . "tc-template.rb")
         ("-spec\\.rb$" . "spec-template.rb")
         ("\\.rb$" . "ruby-template.rb")
         ("\\.el$" . "elisp-template.el")
         )
       auto-insert-alist))
