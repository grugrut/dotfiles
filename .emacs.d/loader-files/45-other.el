(use-package groovy-mode
  :ensure
  :mode (("Jenkinsfile" . groovy-mode)))


(use-package rust-mode
  :ensure
  :defer t
  :config
  (setq-default rust-format-on-save t))

(use-package racer
  :ensure
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :ensure
  :defer t
  :after racer
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-rust-setup))))

(use-package alchemist
  :ensure
  :defer t
  :config
  (setq alchemist-hooks-compile-on-save t))

(use-package elixir-mode
  :ensure
  :defer t
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  )

(use-package flycheck-elixir
  :ensure
  :defer t)

(use-package elixir-yasnippets
  :ensure
  :defer t)
