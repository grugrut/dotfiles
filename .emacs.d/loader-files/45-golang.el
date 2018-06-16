(use-package company-go
  :ensure)

(use-package go-eldoc
  :ensure
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-snippets
  :ensure)

(use-package go-mode
  :ensure
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (setq tab-width 4))))

