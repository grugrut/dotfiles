(use-package web-mode
  :ensure
  :after (web-beautify)
  :init
  (add-hook 'web-mode-hook 'rainbow-mode)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.twig\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-html)
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-css)
  )

(use-package emmet-mode
  :ensure
  :init
  (add-hook 'web-mode-hook 'emmet-mode))


(use-package js2
  :ensure js2-mode
  :after (web-beautify)
  :mode ("\\.js\\'" . js2-mode)
  :config
  (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

(use-package company-tern
  :ensure
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package coffee-mode
  :ensure
  :config
  (setq coffee-tab-width 2))

(use-package php-mode
  :ensure
  :mode ("\\.php\\'" . php-mode))

(use-package web-beautify
  :ensure)

