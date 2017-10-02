(use-package helm
  :diminish ""
  :ensure t
  :init
  (global-unset-key (kbd "C-z"))
  (use-package helm-config
    :init
    (setq helm-command-prefix-key "C-z"))
  (use-package helm-descbinds
    :ensure t
    :bind
    (("C-z d" . helm-descbinds))
    :config
    (helm-descbinds-mode))
  (use-package helm-swoop
    :ensure t
    :bind
    (("C-z w" . helm-swoop)))
  (use-package helm-ghq
    :ensure t
    :bind
    (("C-z g" . helm-ghq)))
  (use-package helm-projectile
    :ensure t
    :bind
    (("C-z p" . helm-projectile))
    :config
    (projectile-mode t)
    (helm-projectile-on)
    (setq projectile-mode-line '(:eval (format " Prj[%s]" (projectile-project-name)))))
  :bind
  (("C-;" . helm-mini)
   ("C-M-z" . helm-resume)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  :config
  (helm-mode t))

