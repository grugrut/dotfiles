;; 時刻表示
(defvar display-time-string-forms
  '(month "/" day " " 24-hours ":" minutes))
(display-time)

(use-package smart-mode-line
  :ensure t
  :disabled t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles/\\.emacs\\.d" ":ED:"))
  (sml/setup))

(use-package spaceline-config
  :ensure spaceline
  :init
  (set-face-foreground 'mode-line-buffer-id nil)
  (set-face-background 'mode-line-buffer-id nil)
  (set-face-foreground 'mode-line nil)
  (set-face-background 'mode-line nil)
  (set-face-attribute 'mode-line nil :box nil)
  :config
  (setq-default powerline-default-separator 'wave
                spaceline-separator-dir-left '(right . right)
                spaceline-separator-dir-right '(left . left)
                mode-line-format '("%e" (:eval (spaceline-ml-main))))
  (spaceline-helm-mode +1)
  (spaceline-compile
    `(((buffer-modified buffer-size)
       :priority 0)
      (anzu :priority 4)
      ((buffer-id remote-host)
       :face highlight-face
       :priority 5)
      major-mode
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 3)
      (minor-modes :when active)
      (version-control :when active :priority 7)
      (org-clock :when active))
    `(which-function
      (selection-info :when mark-active)
      ((buffer-encoding-abbrev point-position line-column)
       :priority 3)
      (global :when active)
      (buffer-position :priority 0)
      (hud :priority 0))))
