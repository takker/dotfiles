;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;* encodingの設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;;* load-path
(add-to-list 'load-path "~/.emacs.d/auto-install")
(add-to-list 'load-path "~/.emacs.d/elisp")

;;* auto-install
;; http://www.emacswiki.org/emacs/auto-install.el
;; FIXME: この部分を後ろに置くとauto-install時にエラー
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)

;;** ~/.emacs.d/elisp 以下のサブディレクトリを再帰的にロードパスに追加
(defun my-add-load-path-subdir (dirlist)
  (with-temp-buffer
    (dolist (dir dirlist)
      (cd dir)
      (normal-top-level-add-subdirs-to-load-path))))

(my-add-load-path-subdir
 '(
   "~/.emacs.d/elisp"
   ))

;;* init-loader
;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
;;** デフォルトで"~/.emacs.d/inits"以下のファイルをロードする
(require 'init-loader)
(init-loader-load)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(anything-file-name ((t (:foreground "white"))))
 '(anything-header ((t (:background "green" :foreground "black"))))
 '(anything-match ((t (:background "skyblue" :foreground "black"))))
 '(cursor ((t (:background "snow" :foreground "black"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "goldenrod"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod"))))
 '(minibuffer-prompt ((t (:foreground "Green"))))
 '(mode-line ((t (:background "darkgreen" :foreground "gold" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "dimgray" :foreground "white"))))
 '(region ((t (:background "skyblue" :foreground "black")))))
