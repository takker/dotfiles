;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* カラーテーマの変更
(load "zenburn")
(require 'color-theme)
(color-theme-zenburn)

(custom-set-faces
 '(cursor ((t (:background "snow" :foreground "black"))))
 '(region ((t (:background "skyblue" :foreground "black")))) ; リージョン
 '(mode-line ((t (:background "darkgreen" :foreground "gold" :box (:line-width -1 :style released-button))))) ; モードライン
 '(mode-line-inactive ((t (:background "dimgray" :foreground "white"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod")))) ; コメント
 '(font-lock-comment-delimiter-face ((t (:foreground "goldenrod")))) ;コメント区切り
 '(minibuffer-prompt ((t (:foreground "Green")))) ;ミニバッファプロンプト
 ;;** anything関連のフェイス
 '(anything-header ((t (:background "green" :foreground "black"))))
 '(anything-match ((t (:background "skyblue" :foreground "black"))))
 '(anything-file-name ((t (:foreground "white"))))
  )

;;* 括弧の色を変える
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "gray10")
(set-face-foreground 'show-paren-match-face "SkyBlue")

;;* 全角スペース, 行末空白, TAB に色を付ける
;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("^\t+" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ;;("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; 現在行を目立たせる.
(global-hl-line-mode)
(defface my-hl-line-face
  '((((class color) (background dark)) ; カラーかつ, 背景が dark ならば,
     (:background "DarkSlateBlue" t))  ; 背景を黒に.
    (((class color) (background light)) ; カラーかつ, 背景が light ならば,
     (:background "ForestGreen" t))     ; 背景を ForestGreen に.
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)

