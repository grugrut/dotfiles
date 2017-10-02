(use-package web-mode
  :ensure t
  :init (add-hook 'web-mode-hook 'rainbow-mode)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.twig\\'" . web-mode))
  :config (setq web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package js2
  :ensure js2-mode
  :mode ("\\.js\\'" . js2-mode))

(use-package company-tern
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package coffee-mode
  :ensure t
  :config
  (setq coffee-tab-width 2))

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

(use-package web-beautify
  :ensure t
  :config
  (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

