(use-package web-mode
  :ensure
  :after (web-beautify)
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'rainbow-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-style-padding 1
        web-mode-script-padding 1)
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-html)
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-css)
  )

(use-package emmet-mode
  :ensure
  :commands (emmet-mode)
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
  :commands tern-mode
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package coffee-mode
  :ensure
  :defer t
  :config
  (setq coffee-tab-width 2))

(use-package php-mode
  :ensure
  :mode ("\\.php\\'" . php-mode))

(use-package web-beautify
  :ensure
  :defer t)

