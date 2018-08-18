(use-package rust-mode
  :ensure
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  :config
  (setq rust-format-on-save t)
  )

(use-package racer
  :ensure
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
