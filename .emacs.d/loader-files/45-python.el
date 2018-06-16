(use-package python-mode
  :ensure
  :mode (("\\.py\\'" . python-mode))
  :config
  (use-package py-autopep8
    :ensure
    :init
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
  (bind-key "C-c C-c" 'quickrun python-mode-map)
  ;;(setq python-shell-interpreter "python")
  ;;(add-to-list 'python-shell-completion-native-disabled-interpreters "python"))
)

(use-package jedi-core
  :ensure
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (use-package company-jedi
    :ensure)
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:complete-on-dot t))

