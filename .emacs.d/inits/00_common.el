;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil; -*-
;;* 自己紹介
(setq user-full-name "KASHIO Takaaki"
      user-mail-address "t.kashio@gmail.com")

;;* Common Lisp extensions for Emacs(use it anyway).
(require 'cl)

;;* Start server
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
;;** 編集が終了したらEmacsをアイコン化する
(add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
;;** C-x C-c => server-edit
(global-set-key [(C x) (C c)] 'server-edit)
;;** M-x exitでEmacsを終了できるようにする
(defalias 'exit 'save-buffers-kill-emacs)
;;** emacsclient で Buffer `hogehoge' still has clients; kill it? (yes or no) とかいわれるのがうざいのをなおす
;; http://aki.issp.u-tokyo.ac.jp/itoh/hiChangeLog/html/2007-04.html#2007-04-09-1
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; マクロ定義
;;* http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
(defmacro add-hook-fn (name &rest body)
  `(add-hook ,name #'(lambda () ,@body)))
(defmacro global-set-key-fn (key args &rest body)
  `(global-set-key ,key (lambda ,args ,@body)))
(defmacro append-to-list (to lst)
  `(setq ,to (append ,lst ,to)))

;;* C-h: バックスペース
(global-set-key [(C h)] 'delete-backward-char)

;;* ビープ音を鳴らさない
(setq visible-bell t)

;;* X11とクリップボードを共有
(setq x-select-enable-clipboard t)

;;* 自動セーブしない
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;;* バックアップファイルを作らない
(setq make-backup-files nil)

;;* kill-line で改行文字も削除
(setq kill-whole-line t)

;;* カーソル付近のファイル/URL を開く
(ffap-bindings)

;;* メニューバーとツールバーの表示設定
(menu-bar-mode 0)
(tool-bar-mode 0)

;;* リージョンを表示
(setq transient-mark-mode t)

;;* リージョンをC-hで削除
(delete-selection-mode t)

;;* カーソルの点滅を OFF
(blink-cursor-mode nil)

;;* yes or no を y or n or SPC で答える
(fset 'yes-or-no-p 'y-or-n-p)

;;* 対応する括弧を表示
(show-paren-mode t)

;;* tab-width を 4 に
(setq-default tab-width 4)

;;* フレームタイトルの設定
(setq frame-title-format "%b")

;;* 起動直後の*scratch* buffer に入る文字列をなくす
(setq initial-scratch-message nil)

;;* 起動画面を表示しない
(setq inhibit-startup-message t)

;;* インデント文字としてタブではなくスペースを使う
(setq-default indent-tabs-mode nil)

;;* デフォルトのメジャーモードをtext-modeにする
(setq default-major-mode 'text-mode)

;;* 行間を空ける
(setq-default line-spacing 2)

;;* スクリプトファイルを保存時に自動的に実行可能にする
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;* 縦分割とかでも行を折り返す
(setq truncate-partial-width-windows nil)

;;* 一行ずつスクロール
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;;* スクロール時にカーソル位置を保持
(setq scroll-preserve-screen-position t)

;;* 行番号を表示
(global-linum-mode t)
;;** 行番号のフォーマット
(set-face-attribute 'linum nil :foreground "red" :height 0.9)
(setq linum-format "%4d")

;;* Fill column
(setq default-fill-column 72)

;;* アクティブでないバッファではカーソルを出さない
(setq cursor-in-non-selected-windows nil)

;; ;;* iswitchb
;; (iswitchb-mode 1)
;; ;;** バッファ読み取り関数をiswitchbにする
;; (setq read-buffer-function 'iswitchb-read-buffer)
;; ;;** 新しいバッファを作るときにいちいち聞いてこない
;; (setq iswitchb-prompt-newbuffer nil)

;;* uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;* recenf-mode
;(recentf-mode t)
;;** 最近のファイルを1000個保存する
(setq recentf-max-saved-items 1000)
;;** 最近使ったファイルに加えないファイルを正規表現で指定する
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
;; http://d.hatena.ne.jp/rubikitch/20091224
(require 'recentf-ext)

;;* Narrowing
(put 'narrow-to-region 'disabled nil)

;;* 保存時に現在行を除いて行末の空白を削除する
;; http://madscientist.jp/~ikegami/diary/20071125.html#p02
(defun delete-trailing-whitespace-except-for-the-current-line ()
  "Delete all the trailing whitespace across the current buffer,
except for the current line where the point is. All whitespace
after the last non-whitespace character in a line is deleted. This
respects narrowing, created by \\[narrow-to-region] and friends. A
formfeed is not considered whitespace by this function."
  (interactive "*")
  (let ((opoint (point)) start l)
    (save-match-data
      (save-excursion
        (save-restriction
          (goto-char (point-min))
          (widen)
          (forward-line 0)
          (setq start (point))
          (goto-char opoint)
          (forward-line 0)
          (setq l (1+ (count-lines 1 (point))))
          (goto-char (point-min))
          (while (and (re-search-forward "\\s-$" nil t)
                      (not (= (count-lines (point-min) (point)) l)))
            (skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
            ;; Don't delete formfeeds, even if they are considered whitespace.
            (save-match-data
              (if (looking-at ".*\f")
                  (goto-char (match-end 0))))
            (delete-region (point) (match-end 0))))))))
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-for-the-current-line)

;;* abbrev file (default ~/.abbrev_defs)
(setq abbrev-file-name "~/.emacs.d/etc/.abbrev_defs")
;;** 略語定義ファイルの読込み
;;** .abbrev_defs が存在していなかったら読み込まない
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file abbrev-file-name))
;;** 略称を保存する
(setq save-abbrevs t)
(define-key esc-map " " 'expand-abbrev) ; M-SPC で略称展開

;;* cua-modeを使って矩形選択や挿入などができるようにする
(cua-mode t)
;;** C-c, C-vの乗っ取りを阻止
(setq cua-enable-cua-keys nil)

;;* bookmarkの設定
;;** ブックマークを変更したら即保存する
(setq bookmark-save-flag t)
;;** 最近使ったブックマークを上に持っていく
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))

;;* migemoの設定
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
;;** キャッシュを使用する
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
;;* 辞書の文字コードはUTF-8
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
;; initialization
(migemo-init)

;;; end-of-file

