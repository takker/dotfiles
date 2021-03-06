;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; 10_elisp.el: 各種elispパッケージの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ddskk
;; http://openlab.ring.gr.jp/skk/skk-manual/skk-manual-ja.html
(require 'skk-autoloads)
;;** skkの設定を保存するディレクトリ
(setq skk-user-directory "~/.emacs.d/etc/skk")
;;** skkの設定ファイルを起動時にバイトコンパイルする
(setq skk-byte-compile-init-file t)
;;** skkを起動時に読み込む
(setq skk-preload t)
;;** skk起動時に作動するフック
(defun skk-load-hooks ()
  (require 'context-skk))               ; 文脈に応じた自動モード切り替え

(add-hook 'skk-load-hook 'skk-load-hooks)
(global-set-key [(C x) (C j)] 'skk-mode)
(global-set-key [(C x) (j)] 'skk-auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* sequential-command: 同じコマンドを連続実行
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)
(sequential-command-setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* magit: gitインタフェース
(require 'magit)
;; http://d.hatena.ne.jp/rubikitch/20100716/git
(require 'git-dwim)
;;** キーバインドの設定
;;*** C-x v => magit-status
;;*** C-x l => magit-log
;;*** C-x b => git-branch-next-action
(global-set-key [(C x) (v) (s)] 'magit-status)
(global-set-key [(C x) (v) (l)] 'magit-log)
(global-set-key [(C x) (v) (b)] 'git-branch-next-action)

;;* git-status: Gitで管理しているファイルの状態をモードラインに表示
;; http://d.hatena.ne.jp/kitokitoki/20100824/p1
(require 'git-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* key-chord: 2つのキーの同時押しでコマンド実行
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* auto-complete: 補完候補を自動的に表示
;; http://cx4a.org/software/auto-complete/manual.ja.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)
;;** C-n, C-pで項目の選択を行うようにする
(setq ac-use-menu-map t)
(setq ac-dwim t)
(setq ac-auto-start 3)                  ; 3文字以上入力されたら補完開始

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* popup-kill-ring: kill-ringの内容をポップアップ
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)

(global-set-key [(M y)] 'popup-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* color-moccur: バッファ一覧の検索
(require 'color-moccur)
;;** C-c m => moccur
(global-set-key [(C x) (m)] 'moccur)
(global-set-key [(C x) (d)] 'dmoccur)
;;** moccur で r を押して編集可能にする
(load "moccur-edit")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* highlight-completion: ファイル名補完
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=highlight-completion
(setq hc-ctrl-x-c-is-completion t)
(require 'highlight-completion)
(highlight-completion-mode 1)
(global-set-key "\C-\\" 'toggle-input-method)
; highlight-completion-mode と干渉するので, URL の時には
; 補完しないようにする
(defadvice hc-expand-file-name
  (around hc-expand-file-name-del activate)
  (if name
      ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* word-count: 単語数をカウント,M-+ でマイナーモードを ON
(autoload 'word-count-mode "word-count"
          "Minor mode to count words." t nil)
(global-set-key [(M +)] 'word-count-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* iswitchb の設定
(require 'iswitchb)
(iswitchb-default-keybindings)
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "選択している buffer を window に表示してみる. "
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))
; 選択しているバッファのファイル名などを表示
;; (defvar dired-directory nil)
(defadvice iswitchb-completions (after
                                 iswitchb-completions-with-file-name
                                 activate)
  "選択してるときにファイル名とかを出してみる. "
  (when iswitchb-matches
    (save-excursion
      (set-buffer (car iswitchb-matches))
      (setq ad-return-value
            (concat ad-return-value
                    "\n"
                    (cond ((buffer-file-name)
                           (concat "file: "
                                   (expand-file-name (buffer-file-name))))
                          ((eq major-mode 'dired-mode)
                           (concat "directory: "
                                   (expand-file-name dired-directory)))
                          (t
                           (concat "mode: " mode-name " Mode"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ibuffer.el バッファの機能拡張
(require 'ibuffer)
(define-key ctl-x-map "\C-l" 'ibuffer)
(setq ibuffer-formats
      '((mark modified read-only " " (name 30 30)
              " " (size 6 -1) " " (mode 16 16) " " filename)
        (mark " " (name 30 -1) " " filename)))
;;;*** R で指定したバッファを削除
(defun Buffer-menu-grep-delete (str)
  (interactive "sregexp:")
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward str nil t)
      (save-excursion
        (ibuffer-mark-for-delete nil)
        )
      (end-of-line))))
(define-key ibuffer-mode-map "R" 'Buffer-menu-grep-delete)
;;;*** C-, C-. でバッファの移動
(setq my-ignore-buffer-list
      '("*Help*" "*Compile-Log*" "*Mew completions*" "*Completions*"
        "*Shell Command Output*" "*Apropos*" "*Buffer List*" "*scratch*"
        "*anything*" "*Messages*" "*info*" "*Calculator*"
        "*WoMan-Log*" "*twittering*"))
(defun my-visible-buffer (blst)
  (let ((bufn (buffer-name (car blst))))
    (if (or (= (aref bufn 0) ? ) (member bufn my-ignore-buffer-list))
        (my-visible-buffer (cdr blst)) (car blst))))
(defun my-grub-buffer ()
  (interactive)
  (switch-to-buffer (my-visible-buffer (reverse (buffer-list)))))
(defun my-bury-buffer ()
  (interactive)
  (bury-buffer)
  (switch-to-buffer (my-visible-buffer (buffer-list))))
(global-set-key [?\C-,] 'my-grub-buffer)
(global-set-key [?\C-.] 'my-bury-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* auto-save-buffers: 全バッファの自動保存
(require 'auto-save-buffers)
(run-with-idle-timer 2.0 t 'auto-save-buffers) ; 2秒ごとに自動保存

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* hiwin: 現在のバッファを目立たせる
(require 'hiwin)
;;** hiwin-modeを有効にする
(hiwin-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* sdic-inline: カーソル位置にある単語を自動で翻訳しミニバッファに表示
;; http://d.hatena.ne.jp/khiker/20100303/sdic_inline
;;* C-c C-p => カーソル下の単語の意味をツールチップを使って表示
(require 'sdic-inline)
(sdic-inline-mode t)   ; sdic-inline モードの起動
;;** 辞書ファイルの設定
(setq sdic-inline-eiwa-dictionary "/usr/share/dict/gene.sdic"
      sdic-inline-waei-dictionary "/usr/share/dict/jedict.sdic")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* thing-opt: thingを活用してリージョン選択を効率よく行う
;; http://dev.ariel-networks.com/articles/emacs/part5/
(require 'thing-opt)
(define-thing-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* emacs-keybind: emacsで設定されているキーバインドを視覚的に表示
;; http://d.hatena.ne.jp/tuto0621/20090315/1237133124
(require 'emacs-keybind)
;;** 解析に使用するrubyスクリプトのパス
(setq emacs-keybind-program-file "~/.emacs.d/elisp/emacs-keybind/emacs_keybind.rb")
;;** キーボード種類(ascii or japanese)
(setq emacs-keybind-keyboard-kind "japanese")
;;** emacs-keybind.el が自動で生成するファイルの置き場所
(setq emacs-keybind-work-dir "~/Documents/emacs-keybind")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* disk: バッファの状態に応じてsave,reload,openを切り替える
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=disk
(global-set-key [(C x)(C s)] 'disk)
(autoload 'disk "disk" "Save, revert, or find file." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* eldoc: 関数・変数のヘルプをエコーエリアに表示
(require 'eldoc-extension)
(require 'c-eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;;;;;;;;;;;;;;;;;;;;;;
;;* open-junk-file: 使い捨てのファイルを開く
(require 'open-junk-file)
;;** 使い捨てファイルのフォーマット
(setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S.")
(global-set-key (kbd "C-x C-S-f") 'open-junk-file)

;;;;;;;;;;;;;;;;;;;;;;;
;;* summarye: バッファのサマリを表示する
(require 'summarye)
(global-set-key (kbd "<f12>") 'se/make-summary-buffer)
