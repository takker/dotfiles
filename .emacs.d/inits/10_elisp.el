;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;; 10_elisp.el: 各種elispパッケージの設定

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ibus-mode
;; http://d.hatena.ne.jp/supermassiveblackhole/20100609/1276059762
(require 'ibus)
;;** Turn on ibus-mode automatically after loading .emacs
(add-hook 'after-init-hook 'ibus-mode-on)
;; ;;** Use C-SPC for Set Mark command
;; (ibus-define-common-key ?\C-\s nil)
;;** Use C-/ for Undo command
(ibus-define-common-key ?\C-/ nil)
;;** Change cursor color depending on IBus status
(setq ibus-cursor-color
  '("limegreen" "white" "blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* sequential-command: 同じコマンドを連続実行
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)
(sequential-command-setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* magit: gitインタフェース
(require 'magit)
; http://d.hatena.ne.jp/rubikitch/20100716/git
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
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map [(C n)] 'ac-next)     ; C-nで次の項目を選択
(define-key ac-complete-mode-map [(C-p)] 'ac-previous) ; C-pで前の項目を選択
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
;;* toggle: 開いているファイルと関連するファイルとのトグル
; (require 'toggle)

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
;;* mcomplete: M-xの補完
;; (require 'mcomplete)
;; (require 'cl)
;; (load "mcomplete-history")
;; (turn-on-mcomplete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* auto-save-buffers: 全バッファの自動保存
(require 'auto-save-buffers)
(run-with-idle-timer 2.0 t 'auto-save-buffers) ; 2秒ごとに自動保存

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* grep-edit: grepの結果を編集
(require 'grep)
(require 'grep-edit)
;;** http://d.hatena.ne.jp/rubikitch/20081025/1224869598
;; (defadvice grep-edit-change-file (around inhibit-read-only activate)
;;   ""
;;   (let ((inhibit-read-only t))
;;     ad-do-it))
;; ;; (progn (ad-disable-advice 'grep-edit-change-file 'around 'inhibit-read-only) (ad-update 'grep-edit-change-file))

;; (defun my-grep-edit-setup ()
;;   (define-key grep-mode-map '[up] nil)
;;   (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
;;   (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
;;   (set (make-local-variable 'inhibit-read-only) t)
;;   )
;; (add-hook 'grep-setup-hook 'my-grep-edit-setup t)

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
