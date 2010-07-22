;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* diredモードでのキーバインドの変更
;;** o => dired-display-file
;;** C-o => other-window
;;** C-s => dired-isearch-filenames
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "o")   'dired-display-file)
            (define-key dired-mode-map (kbd "C-o") 'other-window)
            (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)))

;;* 拡張機能を使用する
(load "dired-x")
(require 'dired-x)
;;** CやDでの再帰操作を可能にする
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
;;** dired を使って、Tで一気にファイルの coding system (漢字) を変換する
(require 'dired-aux)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key (current-local-map) "T"
              'dired-do-convert-coding-system)))
(defvar dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")
(defvar dired-file-coding-system 'no-conversion)
(defun dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))
(defun dired-do-convert-coding-system (coding-system &optional arg)
  "Convert file (s) in specified coding system."
  (interactive
   (list (let ((default (or dired-default-file-coding-system
                            buffer-file-coding-system)))
           (read-coding-system
            (format "Coding system for converting file (s) (default, %s): "
                    default)
            default))
         current-prefix-arg))
  (check-coding-system coding-system)
  (setq dired-file-coding-system coding-system)
  (dired-map-over-marks-check
   (function dired-convert-coding-system) arg 'convert-coding-system t))

;;** ディレクトリを移動してもファイルの並び順を変更しない
(defadvice dired-advertised-find-file
  (around dired-sort activate)
  (let ((sw dired-actual-switches))
    ad-do-it
    (if (string= major-mode 'dired-mode)
        (progn
          (setq dired-actual-switches sw)
          (dired-sort-other dired-actual-switches)))
    ))
(defadvice dired-my-up-directory
  (around dired-sort activate)
  (let ((sw dired-actual-switches))
    ad-do-it
    (if (string= major-mode 'dired-mode)
        (progn
          (setq dired-actual-switches sw)
          (dired-sort-other dired-actual-switches)))
    ))

;;** sで色々なソート。C-u s で名前順に戻る
(load "sorter")
(add-hook 'dired-load-hook
          (lambda ()
            (require 'sorter)))

;;** dired で r を押すとバッファを編集可能にできる
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(add-hook 'dired-mode-hook
          (lambda ()
            (local-unset-key "\C-j")
            (local-unset-key "\C-m")
            (local-unset-key "\C-o")
            (local-set-key "\C-k" 'wdired-next-line)
            (local-set-key "\C-l" 'wdired-previous-line)))

;;** dired で表示されるファイルの内容を別ウィンドウで表示
(require 'bf-mode)
;;** 別ウィンドウに表示するサイズの上限
(setq bf-mode-browsing-size 10)
;;** 別ウィンドウに表示しないファイルの拡張子
(setq bf-mode-except-ext '("\\.exe$" "\\.com$"))
;;** 容量がいくつであっても表示して欲しいもの
(setq bf-mode-force-browse-exts
      (append '("\\.texi$" "\\.el$")
              bf-mode-force-browse-exts))
;;** 圧縮されたファイルを表示
(setq bf-mode-archive-list-verbose t)
;;** ディレクトリ内のファイル一覧を表示
(setq bf-mode-directory-list-verbose t)

;;** dired で X を押した際の各拡張子に対するデフォルトコマンドの設定
(setq dired-guess-shell-alist-user
      '(("\\.tar\\.gz$"  "tar ztvf")
        ("\\.taz$" "tar ztvf")
        ("\\.tar\\.bz2$" "tar Itvf")
        ("\\.zip$" "unzip -l")
        ("\\.\\(g\\|\\) z$" "zcat")
	("\\.pdf$" "pdfopen --file")))
;;** dired-mode で z を押すと拡張子に関連付けられたソフトで開くように設定
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map
              "z" 'dired-fiber-find)))

(defun dired-fiber-find ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (start-process "explorer" "diredfiber" "explorer.exe"
                       (expand-file-name file))
      (start-process "fiber" "diredfiber" "fiber.exe" file))))
;;** スペースでマークをトグルする (FD like)
(define-key dired-mode-map " " 'dired-toggle-mark)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  ;; S.Namba Sat Aug 10 12:20:36 1996
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    (dired-previous-line 1)))
