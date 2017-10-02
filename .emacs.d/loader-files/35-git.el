(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; gitの差分を表示する
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))

