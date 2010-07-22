;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;;;** shel-pop: Shellとのトグル
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 40.0)

(global-set-key [(C t)] 'shell-pop)
(add-hook 'term-mode-hook
         '(lambda ()
            ; C-h: term 内文字削除にする
            ; C-y: term 内ペーストにする
            (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "C-t") 'shell-pop)
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
            ))

;;;** shell の command を補完可能にする
; (require 'shell-command)
(shell-command-completion-mode)
