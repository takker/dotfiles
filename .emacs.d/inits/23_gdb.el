;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;* gdbを複数のウィンドウで動かす
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
;;* 変数の上にマウスカーソルを置くと値を表示
(add-hook-fn 'gdb-mode-hook (gud-tooltip-mode t))
;;* I/O バッファを表示
(setq gdb-use-separate-io-buffer t)
;;* t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)
;;* gdbを複数のウィンドウで動かす
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
;;* 変数の上にマウスカーソルを置くと値を表示
(add-hook-fn 'gdb-mode-hook (gud-tooltip-mode t))
;;* I/O バッファを表示
(setq gdb-use-separate-io-buffer t)
;;* t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)
