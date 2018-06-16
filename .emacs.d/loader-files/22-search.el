;; anzu
(use-package anzu
  :ensure
  :bind
  (("M-%" . anzu-query-replace))
  :init
  (global-anzu-mode +1)
  (setq anzu-mode-lighter ""               ;マイナーモードに表示する文字列
        anzu-search-threshold 1000         ;これ以上は件数表示しない
        anzu-cons-mode-line-p nil)         ;件数を表示しない(spaceline対応)
  )

;; migemo
(use-package migemo
  :ensure
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-dictionary (file-truename "~/../../usr/local/share/migemo/utf-8/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

;; avy
(use-package avy
  :ensure
  :bind
  (("C-:" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)))

;; ripgrep
(use-package ripgrep
  :ensure
  :bind (("M-s r" . ripgrep-regexp))
  :config
  (setq ripgrep-arguments '("-S")))

;; 大文字・小文字を区別しないでサーチ（有効：t、無効：nil）
(setq-default case-fold-search nil)

;; インクリメント検索時に縦スクロールを有効化（有効：t、無効：nil）
(setq isearch-allow-scroll nil)

;; C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)

;; C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;; C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda() (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook
 'isearch-mode-hook
 '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
 )
(add-hook
 'isearch-mode-end-hook
 '(lambda() (setq w32-ime-composition-window nil))
 )



