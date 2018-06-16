(use-package ess
  :ensure
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  (setq ess-ask-for-ess-directory nil)
  (use-package ess-site)
  (use-package ess-R-object-popup
    :ensure
    :init
    (bind-key "C-c C-g" 'ess-R-object-popup ess-mode-map))
  (use-package ess-R-data-view
    :ensure))

