;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;; C-x f: describe-face-at-point
; カーソル位置のフェイスを調べる
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))
(global-set-key [(C x)(f)] 'describe-face-at-point)

;; C-c d: my-insert-date
; 日付挿入
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%R:%S+09:00" (current-time))))
(global-set-key [(C c)(d)] 'my-insert-date)

;; C-o: other-window-or-split
; 他のウィンドウに移動(なければ分割)
; http://d.hatena.ne.jp/rubikitch/20100210
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(global-set-key [(C o)] 'other-window-or-split)

;; M-n: scroll-nlines-ahead
;; M-p: scroll-nlines-behind
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)
; n行下にスクロール
(defun scroll-nlines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))
; n行上にスクロール
(defun scroll-nlines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))
(global-set-key [(M p)] 'scroll-nlines-behind)
(global-set-key [(M n)] 'scroll-nlines-ahead)

;; C-a: my-beginning-of-line
; back-to-indentation <-> beginning-of-line
(defun my-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key [(C a)] 'my-beginning-of-line)

;; C-e: my-end-of-line
; 行末に飛ぶ。連続で実行したときは、後ろについている空白とタブを削除
(defun my-end-of-line ()
  (interactive)
  (end-of-line)
  (if (eq last-command this-command)
      (delete-horizontal-space)))
(global-set-key [(C e)] 'my-end-of-line)

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

;;; end-of-file
