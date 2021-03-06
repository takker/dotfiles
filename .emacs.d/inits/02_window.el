;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil; -*-
;;* ウィンドウシステムの設定
(if (and (window-system)
         (string-match "^23\." emacs-version))
    (progn
      ;;** フォント設定
      (set-default-font "Inconsolata-11")
      (set-face-font 'variable-pitch "Inconsolata-11")
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("Takaoゴシック" . "unicode-bmp"))
      ;;** フレームサイズ
      (setq default-frame-alist
            (append
             '((width  . 80) (height . 35))
             default-frame-alist))
      ;;** font-lock
      (global-font-lock-mode t)
      (setq font-lock-maximum-decoration t)
      (setq fast-lock nil)
      (setq lazy-lock nil)
      (setq jit-lock t)))

;;* 透過
(modify-frame-parameters (selected-frame) '((alpha . 85)))

;;* スクロールバーを非表示
(set-scroll-bar-mode nil)

;;* フリンジ(左右の折り返し表示領域)はいらない
(fringe-mode 'none)

;;; mode-line
;;* モードラインにカーソルがある行数と桁数を表示
(line-number-mode 1)
(column-number-mode 1)

;;* 現在の関数名を表示
(which-function-mode 1)

;;* モードラインのカスタマイズ
;;** モードラインに時間表示
(setq display-time-string-forms
      '((substring year -2) "/" month "/" day " " dayname " " 24-hours ":" minutes))
(display-time)
;;** Isearch を表示しない
(add-hook-fn 'isearch-mode-hook
             (setcar (cdr (assq 'isearch-mode minor-mode-alist)) ""))
;;** Texinfo を短く
(add-hook-fn 'texinfo-mode-hook
             (setq mode-name "Texi"))
;;** scratch バッファの Lisp Interaction を短く
(add-hook-fn 'lisp-interaction-mode-hook
             (setq mode-name "Lisp-Int"))
;;** Emacs-Lisp を短く
(add-hook-fn 'emacs-lisp-mode-hook
             (setq mode-name "Elisp"))
