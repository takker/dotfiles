;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
(setq outline-minor-mode-prefix "\C-c\C-o")

;; elisp-mode
; http://d.hatena.ne.jp/amt/20090102/outlineMode
(add-hook
 ;; 内部トップレベル
 'emacs-lisp-mode-hook
 (lambda ()
   ;; 正規表現パーツ
   (let (
	 (hr "^;;;;;") ; セパレータ
	 (title "^;;; +20[0-9][0-9][01][0-9][0-3][0-9]"); 大見出し
	 (sexp "^([a-z]+") ; S式ではじまる行
	 (c1 "^[ \t]*;;\*")		; (S式内)コメント レベル1
	 (c2 "^[ \t]*;;\**")		; (S式内)コメント レベル2
	 (c3 "^[ \t]*;;\***")		; (S式内)コメント レベル3

	 (b "\\(")
	 (m "\\)\\|\\(")
	 (e "\\)")
	 )
     ;; 正規表現組立
     (setq outline-regexp (concat b hr m title m sexp m c1 m c2 m c3 e ))

     ;; 正規表現マッチして解釈
     (setq outline-level
	   (function
	    (lambda ()
	      (save-excursion
		(looking-at outline-regexp)
		(cond				;レベルを返す
		 ((match-string 1) 1)	; ^;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ...
		 ((match-string 2) 1)	; ^;;; 20060906 emacms-lisp-mode ...
		 ((match-string 3) 2)	; ^(defun my-function
		 ((match-string 4) 3)	; 	;;* コメント
		 ((match-string 5) 4)	; 	;;** サブコメント
		 ((match-string 6) 5)	; 	;;*** サブサブコメント
		 (t 6)	;
      ))))))
   ;; 正規表現パーツ
   (outline-minor-mode t)
   ))

