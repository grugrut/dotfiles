;; 最終行には必ず1行挿入
(setq require-final-newline t)

;; バッファの最後の改行を抑制
(setq next-line-add-newlines nil)

;; hightlight-symbol
(use-package highlight-symbol
  :ensure t
  :bind
  (("C-." . highlight-symbol-at-point)))

;; expand-region
(use-package expand-region
  :ensure t
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :init
  (global-unset-key (kbd "C-t"))
  :config
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
  :ensure t
  :config
  (global-aggressive-indent-mode t))
