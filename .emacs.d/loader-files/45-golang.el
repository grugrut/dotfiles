(use-package company-go
  :ensure
  :defer t)

(use-package go-eldoc
  :ensure
  :defer t
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-snippets
  :ensure
  :defer t)

(use-package go-mode
  :ensure
  :defer t
  :commands (gofmt-before-save)
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (setq tab-width 4))))

