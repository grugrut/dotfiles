;; 最終行には必ず1行挿入
(setq require-final-newline t)

;; バッファの最後の改行を抑制
(setq next-line-add-newlines nil)

;; リージョン選択時にリージョンまるごと削除
(delete-selection-mode t)

;; hightlight-symbol
(use-package highlight-symbol
  :ensure
  :defer t
  :bind
  (("C-." . highlight-symbol-at-point)))

;; expand-region
(use-package expand-region
  :ensure
  :defer t
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

(use-package multiple-cursors
  :ensure
  :after smartrep
  :config
  (global-unset-key (kbd "C-t"))
  (smartrep-define-key global-map "C-t"
                       '(("C-t" . 'mc/mark-next-like-this)
                         ("n"   . 'mc/mark-next-like-this)
                         ("p"   . 'mc/mark-previous-like-this)
                         ("m"   . 'mc/mark-more-like-this-extended)
                         ("u"   . 'mc/unmark-next-like-this)
                         ("U"   . 'mc/unmark-previous-like-this)
                         ("s"   . 'mc/skip-to-next-like-this)
                         ("S"   . 'mc/skip-to-previous-like-this)
                         ("*"   . 'mc/mark-all-like-this)
                         ("a"   . 'mc/mark-all-like-this)
                         ("d"   . 'mc/mark-all-like-this-dwim)
                         ("i"   . 'mc/insert-numbers)
                         ("l"   . 'mc/insert-letters)
                         ("o"   . 'mc/sort-regions)
                         ("O"   . 'mc/reverse-regions))))


;; minibufferのアクティブ時、IMEを無効化
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (deactivate-input-method)))

(use-package aggressive-indent
  :ensure
  :config
  (global-aggressive-indent-mode t))

(use-package google-this
  :ensure
  :defer t
  :config
  (global-set-key (kbd "M-s g") 'google-this-noconfirm))

(use-package smartparens
  :ensure)
(use-package smartparens-config
  :after smartparens
  :config
  (smartparens-global-mode t))

(use-package smooth-scroll
  :ensure
  :defer t
  :diminish ""
  :config
  (smooth-scroll-mode t))

(use-package rainbow-delimiters
  :ensure
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package fontawesome
  :ensure
  :defer t)

(use-package codic
  :ensure
  :defer t)

(use-package pocket-reader
  :ensure
  :defer t)
