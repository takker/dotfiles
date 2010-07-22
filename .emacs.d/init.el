;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;; encodingの設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/auto-install")
;; init-loader
; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
; デフォルトで"~/.emacs.d/inits"以下のファイルをロードする
(require 'init-loader)
(init-loader-load)

;; auto-install
; http://www.emacswiki.org/emacs/auto-install.el
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
