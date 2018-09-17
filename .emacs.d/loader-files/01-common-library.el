;; clを有効に
(use-package cl)

(use-package dash
  :defer t
  :ensure)

(use-package s
  :defer t
  :ensure)

(use-package f
  :defer t
  :ensure)

(use-package ht
  :defer t
  :ensure)

(use-package smartrep
  :defer t
  :ensure)

(use-package quickrun
  :ensure
  :commands (quickrun)
  :init
  (bind-key "C-c C-c" 'quickrun prog-mode-map))
