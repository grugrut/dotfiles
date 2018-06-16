;; clを有効に
(use-package cl)

(use-package dash
  :ensure)

(use-package s
  :ensure)

(use-package f
  :ensure)

(use-package ht
  :ensure)

(use-package smartrep
  :ensure)

(use-package quickrun
  :ensure
  :init
  (bind-key "C-c C-c" 'quickrun prog-mode-map))
