;;; init.el --- My init script -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)

(modify-frame-parameters nil '((wait-for-wm . nil))) ; Xを使う場合の高速化設定らしい


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; パッケージ
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(package-refresh-contents) ;;重たいので手動でやる

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-verbose t
      use-package-expand-minimally byte-compile-current-file
      use-package-compute-statistics t)

;;; diminishが付属しなくなったので手動で入れる
(use-package diminish :straight t)
(use-package bind-key)

(use-package key-chord
  :straight (:host github :repo "zk-phi/key-chord" :branch "master")
  )
(use-package use-package-chords
  :straight t
  :config (key-chord-mode 1))

;; ベンチマーク
(use-package benchmark-init
  :straight t
  :config
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; ライブラリ群
(use-package cl-lib
  :defer t)

(use-package dash
  :straight t
  :defer t)

(use-package s
  :straight t
  :defer t)

(use-package f
  :straight t
  :defer t)

(use-package ht
  :straight t
  :defer t)

(use-package posframe
  :straight t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 全般
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 色をつける
(global-font-lock-mode t)

;; GC
(setq gc-cons-threshold (* 256 1024 1024))
(setq garbage-collection-messages t)

;; バッファの自動掃除
(use-package midnight
  :config
  (midnight-mode))

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

;; ブラウザ設定 WSL限定
(setq browse-url-browser-function 'browse-url-generic)
(defvar browse-url-generic-program  (executable-find (getenv "BROWSER")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 画面
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; フォント設定
;;
;; abcdefghi
;; 012345678
;; あいうえお
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Migu 1M"))

;; 絵文字
;; (unicode-fonts-setup) ; 最初に本コマンドの実行が必要
;; (all-the-icons-install-fonts)
(use-package unicode-fonts
  :straight t)
(use-package all-the-icons
  :straight t)

(add-to-list 'custom-theme-load-path "~/src/github.com/grugrut/doom-manoj-dark-theme.el/")

(use-package doom-themes
  :straight t
  :config
  ;; (load-theme 'doom-dracula t)
  (load-theme 'doom-manoj-dark t)
  ;; (load-theme 'doom-one t)              
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :defer t
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-minor-modes t)
  (doom-modeline-github nil))

;; ツールバーを表示しない
(tool-bar-mode 0)

;; スクロールバーを表示しない
(set-scroll-bar-mode nil)

;; 行番号を表示
(line-number-mode +1)
(column-number-mode +1)

;; 行番号表示(Emacs26以降)
(global-display-line-numbers-mode t)

(use-package beacon
  :straight t
  :diminish ""
  :config
  (beacon-mode 1))

;; タイトルバーにファイル名を表示
(setq frame-title-format "%f")

;; 対応する括弧を光らせる
(show-paren-mode t)
(defvar show-paren-style 'mixed)

;; カーソルを点滅させない
(blink-cursor-mode 0)

;; 単語での折り返し
(global-visual-line-mode t)

;; バッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-lines nil)

;; マウスを避けさせる
(mouse-avoidance-mode 'jump)

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "*[^*]+*"))


(use-package popwin
  :straight t
  :custom
  (popwin:popup-window-position 'bottom))

;; アクティブなバッファがわかるように
(use-package dimmer
  :straight t
  :config
  (dimmer-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 移動
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package backward-forward
  :straight t t
  :config
  (backward-forward-mode 1))

(use-package avy
  :straight t
  :bind
  (("C-:" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :straight t
  :bind
  (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face ((t (:height 2.0)))))

(use-package bm
  :straight t
  :defer t
  :commands (bm-toggle
             bm-next
             bm-previous)
  :bind
  (("C-S-SPC" . bm-toggle)
   ("C-}" . bm-previous)
   ("C-]" . bm-next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 入力・編集
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ddskk
  :straight t
  :defer t
  :bind
  (("C-x C-j" . skk-mode)
   ("C-x j"   . skk-mode))
  :init
  (defvar dired-bind-jump nil)  ; dired-xがC-xC-jを奪うので対処しておく
  :custom
  (skk-use-azik t)                     ; AZIKを使用する
  (skk-azik-keyboard-type 'jp106)      ;
  (skk-tut-file nil)                   ;
  (skk-server-host "localhost")        ;
  (skk-server-portnum 1178)            ;
  (skk-egg-like-newline t)             ; 変換時にリターンでは改行しない
  (skk-japanese-message-and-error t)   ; メッセージを日本語にする
  (skk-auto-insert-paren t)            ; 対応する括弧を自動挿入
  (skk-check-okurigana-on-touroku t)   ;
  (skk-show-annotation t)              ; アノテーションを表示
  (skk-anotation-show-wikipedia-url t) ;
  (skk-show-tooltip nil)               ; 変換候補をインライン表示しない
  (skk-isearch-start-mode 'latin)      ; isearch時にSKKをオフ
  (skk-henkan-okuri-strictly nil)      ; 送り仮名を考慮した変換候補
  (skk-process-okuri-early nil)
  )

;; 操作した際に、操作箇所を強調表示する
(use-package volatile-highlights
  :straight t
  :diminish ""
  :config
  (volatile-highlights-mode t))

;; 最終行には必ず1行挿入
(setq require-final-newline t)

;; バッファの最後の改行を抑制
(setq next-line-add-newlines nil)

;; リージョン選択時にリージョンまるごと削除
(delete-selection-mode t)

(use-package highlight-symbol
  :straight t
  :defer t
  :bind
  (("C-." . highlight-symbol-at-point)))

(use-package expand-region
  :straight t
  :defer t
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

(use-package smartrep
  :straight t)

(use-package multiple-cursors
  :straight t
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

(use-package smooth-scroll
  :straight t
  :diminish ""
  :functions smooth-scroll-mode
  :config
  (smooth-scroll-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 検索
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package google-this
  :straight t
  :defer t
  :bind (("M-s g" . google-this-noconfirm)))

(use-package anzu
  :straight t
  :bind
  (("M-%" . anzu-query-replace))
  :config
  (global-anzu-mode +1)
  )

(use-package migemo
  :straight t
  :functions migemo-init
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary (file-truename "/usr/share/cmigemo/utf-8/migemo-dict"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init))

(use-package ripgrep
  :straight t
  :defer t
  :bind (("M-s r" . ripgrep-regexp))
  :config
  (setq ripgrep-arguments '("-S")))

;; 大文字・小文字を区別しないでサーチ（有効：t、無効：nil）
(setq case-fold-search nil)

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
  '(lambda()(interactive) (isearch-done)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; コーディング
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq comment-style 'extra-line)

(use-package yafolding
  :straight t t
  :defer t
  :hook
  (prog-mode . yafolding-mode))

(use-package which-function
  :defer t
  :hook
  (prog-mode . which-function-mode))

(use-package projectile
  :straight t t
  :init
  :config
  (setq projectile-mode-line-prefix " Prj")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package quickrun
  :straight t
  :defer t
  :commands (quickrun)
  :init
  (bind-key "C-c C-c" 'quickrun prog-mode-map))

(use-package indent-guide
  :straight t
  :defer t
  :diminish ""
  :config
  (defvar indent-guide-delay 0.1)
  (defvar indent-guide-recursive t)
  :hook (prog-mode . indent-guide-mode))

;; vi風に空行に~を表示する
(use-package vi-tilde-fringe
  :straight t
  :defer t
  :commands vi-tilde-fringe-mode
  :diminish ""
  :hook
  (prog-mode . vi-tilde-fringe-mode))

(use-package aggressive-indent
  :straight t
  :diminish ""
  :hook
  (prog-mode . aggressive-indent-mode))

(use-package minimap
  :disabled t
  :straight t
  :defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0.2
        minimap-minimum-width 20)
  :hook
  (prog-mode . minimap-mode))

(use-package rainbow-mode
  :straight t
  :defer t
  :hook
  (web-mode . rainbow-mode))

(use-package neotree
  :straight t)

(use-package flycheck
  :straight t
  :defer t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode))

;; (use-package eglot
;;   :straight t
;;   :defer t
;;   :config
;;   (add-to-list 'eglot-server-programs '(go-mode . ("go-langserver"
;;                                                    "-mode=stdio"
;;                                                    "-gocodecompletion"
;;                                                    "-diagnostics"
;;                                                    "-lint-tool=golint")))
;;   (add-to-list 'eglot-server-programs '(elixir-mode . ("language_server.sh")))
;;   ;; (elixir-mode . ("language_server.sh"))))
;;   (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
;;   :hook
;;   (go-mode . eglot-ensure)
;;   (python-mode . eglot-ensure)
;;   (web-mode . eglot-ensure)
;;   (js2-mode . eglot-ensure)
;;   (elixir-mode . eglot-ensure))

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  (web-mode . lsp)
  (js2-mode . lsp)
  (elixir-mode . lsp))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c r") 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c i") 'lsp-ui-imenu)
  (setq lsp-ui-doc-position 'bottom))

(use-package company-lsp
  :straight t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package go-mode
  :straight t
  :defer t
  :commands (gofmt-before-save)
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4))

(use-package go-impl
  :straight t
  :defer t
  :commands go-impl)

(use-package web-mode
  :straight t
  :defer t
  :after flycheck
  :functions flycheck-add-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-style-padding 1
        web-mode-script-padding 1)
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-html)
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-css)
  )

(use-package emmet-mode
  :straight t
  :defer t
  :commands (emmet-mode)
  :hook
  (web-mode . emmet-mode))

(use-package js2-mode
  :straight t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :bind (:map js2-mode-map
              ("C-c b" . web-beautify-js))
  )

(use-package coffee-mode
  :straight t
  :defer t
  :config
  (setq coffee-tab-width 2))

(use-package php-mode
  :straight t
  :defer t
  :mode ("\\.php\\'" . php-mode))

(use-package web-beautify
  :straight t
  :defer t)

(use-package groovy-mode
  :straight t
  :defer t
  :mode (("Jenkinsfile" . groovy-mode)))

(use-package rust-mode
  :straight t
  :defer t
  :config
  (setq-default rust-format-on-save t))

(use-package racer
  :straight t
  :defer t
  :hook
  (rust-mode . racer-mode)
  (racer-mode . eldoc-mode))

(use-package flycheck-rust
  :straight t
  :defer t
  :after racer
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-rust-setup))))

(use-package alchemist
  :straight t
  :defer t
  :config
  (setq alchemist-hooks-compile-on-save t))

(use-package elixir-mode
  :straight t
  :defer t
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  )

(use-package flycheck-elixir
  :straight t
  :defer t
  :after elixir-mode)

(use-package elixir-yasnippets
  :straight t
  :defer t
  :after elixir-mode)

(use-package python-mode
  :straight t
  :defer t
  :mode (("\\.py\\'" . python-mode))
  :config
  (bind-key "C-c C-c" 'quickrun python-mode-map)
  )

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package markdown-mode
  :straight t
  :defer t
  :mode ("\\.md\\'" . gfm-mode))

;; minibufferのアクティブ時、IMEを無効化
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (deactivate-input-method)))

(use-package smartparens
  :straight t
  :defer t
  :diminish "")
(use-package smartparens-config
  :defer t
  :after smartparens
  :hook
  (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package fontawesome
  :straight t)

(use-package codic
  :straight t
  :defer t)

(use-package pocket-reader
  :straight t
  :defer t)

(use-package company
  :straight t
  :diminish company-mode
  :functions (global-company-mode
              company-abort
              company-complete-number)
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-begin-commands '(self-insert-command)
        company-selection-wrap-around t
        company-show-numbers t))

(use-package company-box
  :straight t t
  :diminish
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-elisp
        (list
         (concat (all-the-icons-material "functions") " ")
         (concat (all-the-icons-material "check_circle") " ")
         (concat (all-the-icons-material "stars") " ")
         (concat (all-the-icons-material "format_paint") " ")))
  (setq company-box-icons-unknown (concat (all-the-icons-material "find_in_page") " "))
  (setq company-box-backends-colors nil))

(use-package company-posframe
  :straight t t
  :diminish
  :after company
  :config
  (company-posframe-mode 1))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :functions yas-global-mode
  :config
  (yas-global-mode 1))

(use-package view
  :defer t
  :chords (("jk" . view-mode))
  :config
  (setq view-read-only t)
  (bind-keys :map view-mode-map
             ("j" . next-line)
             ("k" . previous-line)
             ("h" . backward-char)
             ("l" . forward-char)))

(use-package org
  :defer t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :mode ("\\.org$'" . org-mode)
  :hook  (org-mode . (lambda ()
                       (set (make-local-variable 'system-time-locale) "C")))
  :custom
  (org-directory "~/org/")
  ;; TODO状態の設定
  (org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
  (org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                            ("STARTED" :foreground "cornflower blue" :weight bold)
                            ("DONE" :foreground "green" :weight bold)
                            ("WAITING" :foreground "orange" :weight bold)
                            ("HOLD" :foreground "magenta" :weight bold)
                            ("CANCELLED" :foreground "green" :weight bold)
                            ("MEETING" :foreground "gren" :weight bold)))
  ;; 時間計測を開始したらSTARTED状態に
  (org-clock-in-switch-to-state 'my-org-clock-in-switch-to-state)
  (org-log-done 'time)
  (org-clock-persist t)
  (org-clock-out-when-done t)
  )

(use-package org-capture
  :defer t
  :after org
  :commands (org-capture)
  :custom
  (org-capture-templates `(
                           ("t" "Todo" entry
                            (file ,(concat org-directory "todo.org"))
                            "* TODO %?\n %i\n %a\n"
                            :prepend nil
                            :unnarrowed nil
                            :kill-buffer t
                            )
                           ("m" "Memo" entry
                            (file+datetree ,(concat org-directory "diary.org"))
                            "* %?\n %a"
                            :prepend t
                            :unnarrowed nil
                            :kill-buffer t
                            )
                           ("i" "interrupt" entry
                            (file+datetree ,(concat org-directory "diary.org"))
                            "* PHONE %?\n %a"
                            :prepend t
                            :unnarrowed nil
                            :kill-buffer t
                            :clock-in t
                            :clock-resume t
                            )
                           ("b" "blog" entry
                            (file+headline "~/src/github.com/grugrut/til/draft/blog.org" ,(format-time-string "%Y"))
                            "** TODO %?\n:PROPERTIES:\n:EXPORT_HUGO_SECTION: %(format-time-string \"%Y/%m\")\n:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :archives '(%(format-time-string \"%Y\") %(format-time-string \"%Y/%m\"))\n:EXPORT_FILE_NAME: %(format-time-string \"%Y%m%d%H%M%S\")\n:END:\n")
                           )))

;;; TODOの場合だけSTARTEDに変更する
(defun my-org-clock-in-switch-to-state (state)
  "Change state to STRTED when previous STATE is only TODO."
  (when (string-equal state "TODO")
    "STARTED"))

(use-package org-bullets
  :disabled t
  :straight t
  :hook
  (org-mode (lambda () (org-bullets-mode 1))))

;;; #+UPDATE:を保存時に更新
(use-package time-stamp
  :disabled t
  :init
  (add-hook 'before-save-hook 'time-stamp)
  :config
  (setq time-stamp-active t
        time-stamp-line 10
        time-stamp-start "^#\\+LASTMOD:"
        time-stamp-format " %:y-%02m-%02d"
        time-stamp-end "$"))

(use-package ox-hugo
  :straight t
  :after ox
  :mode ("\\.org$'" . org-hugo-auto-export-mode))

(use-package ob
  :defer t
  :after org
  :functions org-babel-do-load-languages
  :config
  (use-package ob-elixir
    :straight t)
  (use-package ob-go
    :straight t)
  (use-package ob-rust
    :straight t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)
     (go . t)
     (rust . t))))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

;; gitの差分を表示する
(use-package git-gutter-fringe
  :straight t
  :hook
  (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:lighter . nil))

(use-package helm
  :diminish helm-mode
  :straight t
  :init
  (global-unset-key (kbd "C-z"))
  (use-package helm-config
    :init
    (setq helm-command-prefix-key "C-z"))
  (use-package helm-descbinds
    :straight t
    :bind
    (("C-z d" . helm-descbinds))
    :config
    (helm-descbinds-mode))
  (use-package helm-swoop
    :straight t
    :bind
    (("C-z w" . helm-swoop)))
  (use-package helm-ghq
    :straight t
    :bind
    (("C-z g" . helm-ghq)))
  (use-package helm-ag
    :straight t
    :bind
    (("C-z ;" . helm-ag))
    :config
    (setq helm-ag-base-command "rg -S --no-heading"))
  (use-package helm-projectile
    :straight t
    :config
    (helm-projectile-on))
  (use-package helm-git-grep
    :straight t
    :bind
    (("C-z s" . helm-git-grep)))
  :bind
  (("C-;" . helm-mini)
   ("C-M-z" . helm-resume)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  :config
  (helm-mode t))

(use-package helm-posframe
  :straight t
                                        ;  :config
                                        ;(helm-posframe-enable)
  :custom
  (helm-posframe-poshandler 'posframe-poshandler-frame-center)
  (helm-posframe-parameters '((background-color . "gray50"))))


;; C-hをバックスペース
(keyboard-translate ?\C-h ?\C-?)

;; which-key
(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))
(use-package which-key-posframe
  :straight t
  :config
  (which-key-posframe-mode)
  :custom
  (which-key-posframe-border-width 2))

(use-package keyfreq
  :straight t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun my/copy-now-line ()
  "現在ファイルと行をコピーする."
  (interactive)
  (let ((text (format "%s L%d" (buffer-name) (line-number-at-pos))))
    (message text)
    (kill-new text)))

;;; サスペンドさせない
(global-unset-key (kbd "C-x C-z"))

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; init.el ends here
