(add-hook-fn 'help-mode-hook
             (define-key help-mode-map (kbd "M-[") 'help-go-back)
             (define-key help-mode-map (kbd "M-]") 'help-go-forward))
