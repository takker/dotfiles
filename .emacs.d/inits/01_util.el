;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* C-x f => カーソル位置のフェイスを調べる
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))
(global-set-key [(C x)(f)] 'describe-face-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-c d => 日付挿入
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%R:%S+09:00" (current-time))))
(global-set-key [(C c)(d)] 'my-insert-date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-o => 他のウィンドウに移動(なければ分割)
;;** http://d.hatena.ne.jp/rubikitch/20100210
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(global-set-key [(C o)] 'other-window-or-split)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-x % => 対応する括弧にジャンプ
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1))))
  )
(global-set-key [(C x) (%)] 'match-paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 前回ジャンプした文字と方向を覚えておくための変数
(defvar last-search-char " ")
(defvar last-search-direction 'forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-z => 指定した文字(前方)にジャンプ
(defun move-to-char-forward (arg char)
  "Move forward to the provided character."
  (interactive "p\ncMove to char: ")
  (forward-char 1)
  (if (search-forward (string char) nil t arg)
      (setq last-search-char char
            last-search-direction 'forward)
    (message "Search failed: \"%c\"" char))
  (backward-char 1))
(global-set-key (kbd "C-z") 'move-to-char-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-S-z => 指定した文字(後方)にジャンプ
(defun move-to-char-backward (arg char)
  "Move backward to the provided character."
  (interactive "p\ncMove backward to char: ")
  (if (search-backward (string char) nil t arg)
      (setq last-search-char char
            last-search-direction 'backward)
    (message "Search failed: \"%c\"" char)))
(global-set-key (kbd "C-S-z") 'move-to-char-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-; => 前回ジャンプした文字に繰り返しジャンプ
(defun repeat-move-to-char (arg)
  "Repeat move-to-char toward the char and the direction last jumped."
  (interactive "p")
  (if (eq last-search-direction 'forward)
      (move-to-char-forward arg last-search-char)
    (move-to-char-backward arg last-search-char)))
(global-set-key (kbd "C-;") 'repeat-move-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* M-z => zap-to-charの改良版
;;** http://www.emacswiki.org/emacs/ZapUpToChar
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-S-h => 現在のウィンドウで表示されている最初の行に移動
;;* C-S-m => 現在のウィンドウで表示されている中央の行に移動
;;* C-S-l => 現在のウィンドウで表示されている最後の行に移動
(global-set-key-fn (kbd "C-S-h") nil (interactive)
                   (move-to-window-line 0)
                   (recenter))
(global-set-key-fn (kbd "C-S-m") nil (interactive)
                   (move-to-window-line nil)
                   (recenter))
(global-set-key-fn (kbd "C-S-l") nil (interactive)
                   (move-to-window-line -1)
                   (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ポイントが行の最後にある場合、行全体をkill-line
(defadvice kill-line (before kill-whole-line-if-end
                             (&optional arg))
  "Kill the line if the point is on the end of line."
  (if (eolp) (beginning-of-line)))
(ad-activate 'kill-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* M-o => 現在行の上に新しい行を追加
(defadvice open-line (before open-whole-line
                             (&optional arg))
  "Insert newline before the line and leave the point."
  (beginning-of-line 1))
(ad-activate 'open-line)
(global-set-key [(M o)] 'open-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* M-S-o => 現在行の次に新しい行を追加
(defun open-next-line (&optional arg)
  "Insert a newline into the next line and leave point before it."
  (interactive "p")
  (next-line 1)
  (open-line arg))
(global-set-key [(M S o)] 'open-next-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* C-w => kill-region or backward-word
;; リージョンが活性化していればリージョン削除
;; 非活性であれば、直前の単語を削除
(defun kill-region-or-backward-kill-word (&optional arg)
  "Kill region if it is activated, or kill bacward word."
  (interactive "p")
  (if (region-active-p)
      (kill-region (point) (mark))
    (backward-kill-word arg)))
(global-set-key [(C w)] 'kill-region-or-backward-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* 他のユーザ所有のファイルをsudoで開き直す
;; http://ubulog.blogspot.com/2010/08/emacs-sudo2.html
(defun file-other-p (filename)
  "Return t if file FILENAME created by others."
  (if (file-exists-p filename)
      (/= (user-real-uid) (nth 2 (file-attributes filename))) t))

(defun file-username (filename)
   "Return File Owner."
   (if (file-exists-p filename)
      (user-full-name (nth 2 (file-attributes filename)))
     (user-full-name (nth 2 (file-attributes (file-name-directory filename))))))

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-other-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0) " is "
                             (if (file-exists-p (ad-get-arg 0)) "read-only." "newer file.")
                             "  Open it as "
                                     (file-username (ad-get-arg 0)) "? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
    (set-buffer (find-file (concat "/sudo:"
                                   (file-username file) "@" (system-name) ":" file))))

;;; end-of-file
