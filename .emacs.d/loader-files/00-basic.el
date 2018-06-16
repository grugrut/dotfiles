
;; 色をつける
(global-font-lock-mode t)

;; GC
(setq gc-cons-threshold (* gc-cons-threshold 100))
(setq garbage-collection-messages t)

;; バッファの自動掃除
(use-package midnight)

;; 大きなファイルを開くときの警告を出にくくする
(setq large-file-warning-threshold (* 25 1024 1024))

;; ファイルを開く際の大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t)

;; yes-or-no-pをy/nで選択できるようにする
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; history
(setq history-length 500
      history-delete-duplicates t)

;; recentf
(defvar recentf-max-saved-items 1000)
(defvar recentf-auto-cleanup 'never)

;; move physical line
(setq line-move-visual nil)

;; マウスでコピーできるように
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; ベルを鳴らさない
(setq ring-bell-function (lambda()))

;; タブはスペースで
(setq-default indent-tabs-mode nil)

;; バックアップを作成しない
(setq backup-inhibited t)
;; 初期画面を表示しない
(setq inhibit-startup-message t)

(setq tramp-debug-buffer t)
(setq tramp-verbose 10)
(setq password-cache-expiry nil)

;; クリップボードを監視して自動貼り付け
(use-package clipmon
  :ensure)

;; ブラウザ設定
(setq browse-url-generic-program
      (executable-find (getenv "BROWSER"))
      browse-url-browser-function 'browse-url-generic)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))
