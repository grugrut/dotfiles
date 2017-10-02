(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  (setq ess-ask-for-ess-directory nil)
  (use-package ess-site)
  (use-package ess-R-object-popup
    :ensure t
    :init
    (bind-key "C-c C-g" 'ess-R-object-popup ess-mode-map))
  (use-package ess-R-data-view
    :ensure t))
 

