(use-package view
  :ensure t
  :defer t
  :init
  (setq view-read-only t)
  :config
  (bind-keys :map view-mode-map
             ("j" . next-line)
             ("k" . previous-line)
             ("h" . backward-char)
             ("l" . forward-char)))

