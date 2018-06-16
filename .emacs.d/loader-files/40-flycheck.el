(use-package flycheck
  :ensure
  :diminish ""
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :disabled t
    :ensure t
    :init
    (flycheck-pos-tip-mode)))
 
