;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* C-x f => カーソル位置のフェイスを調べる
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))
(global-set-key [(C x)(f)] 'describe-face-at-point)

;; C-c d => 日付挿入
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%R:%S+09:00" (current-time))))
(global-set-key [(C c)(d)] 'my-insert-date)

;;* C-o => 他のウィンドウに移動(なければ分割)
;;** http://d.hatena.ne.jp/rubikitch/20100210
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(global-set-key [(C o)] 'other-window-or-split)

;;* M-n => n行下にスクロール
;;* M-p => n行上にスクロール
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)
(defun scroll-nlines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))
(defun scroll-nlines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))
(global-set-key [(M p)] 'scroll-nlines-behind)
(global-set-key [(M n)] 'scroll-nlines-ahead)

;; %: match-paren
; 対応する括弧にジャンプ
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1))))
  )
(global-set-key [(%)] 'match-paren)

;;* C-z => 指定した文字にジャンプ
(defun move-to-char (arg char)
  "Move to the provided character."
  (interactive "p\ncMove to char: ")
    (forward-char 1)
    (if (search-forward (char-to-string char) nil t arg)
        t
      (message "Search failed: \"%c\"" char))
    (backward-char 1))
(global-set-key (kbd "C-z") 'move-to-char)

;;* M-z => zap-to-charの改良版
;;** http://www.emacswiki.org/emacs/ZapUpToChar
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;;; end-of-file
