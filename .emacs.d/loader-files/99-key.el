;; C-hをバックスペース
(keyboard-translate ?\C-h ?\C-?)

;; sequential-command
(use-package sequential-command-config
  :ensure sequential-command
  :config
  (sequential-command-setup-keys))

;; which-key
(use-package which-key
  :ensure
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

;; 同時押し
(use-package key-chord
  :ensure
  :config
  (key-chord-mode 1)
  (defvar key-chord-two-keys-delay 0.04)
  (key-chord-define-global "jk" 'view-mode))
  

(use-package keyfreq
  :ensure
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
