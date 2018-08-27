(use-package groovy-mode
  :ensure
  :mode (("Jenkinsfile" . groovy-mode)))


(use-package rust-mode
  :ensure
  :config
  (setq-default rust-format-on-save t))

(use-package racer
  :ensure
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :ensure
  :after racer
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-rust-setup))))

(use-package alchemist
  :ensure
  :config
  (setq alchemist-hooks-compile-on-save t))

(use-package elixir-mode
  :ensure
  :init
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  )

(use-package flycheck-elixir
  :ensure)

(use-package elixir-yasnippets
  :ensure)
