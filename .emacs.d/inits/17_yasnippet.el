;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-
;; (add-to-list 'load-path
;;              "~/.emacs.d/plugins")
;; (require 'yasnippet-bundle)

;; yasunippet normal install
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
(yas/load-directory "~/.emacs.d/plugins/yasnippets-rails/rails-snippets")
