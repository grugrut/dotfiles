;;; init.el --- My init script -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(modify-frame-parameters nil '((wait-for-wm . nil))) ; Xを使う場合の高速化設定らしい

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; パッケージ
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))
(package-initialize)
;;(package-refresh-contents) ;;重たいので手動でやる

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; diminishが付属しなくなったので手動で入れる
(use-package diminish :ensure)

;; ベンチマーク
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package auto-async-byte-compile
  :ensure
  :config
  (setq auto-async-byte-compile-init-file "~/.emacs.d/init.el")
  :hook ((emacs-lisp--mode . enable-auto-async-byte-compile-mode)))

;;; ライブラリ群
(use-package cl-lib)

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

;; ブラウザ設定 WSL限定
(setq  browse-url-browser-function 'browse-url-generic
       browse-url-generic-program  (executable-find (getenv "BROWSER")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 画面
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'manoj-dark)

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
  :ensure
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

;; フォント設定
;;
;; abcdefghi
;; 012345678
;; あいうえお
(set-face-attribute 'default nil
                    :family "Migu 1M"
                    :height 140)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Migu 1M"))

;; 絵文字
;; (unicode-fonts-setup) ; 最初に本コマンドの実行が必要
;; (all-the-icons-install-fonts)
(use-package unicode-fonts
  :ensure)
(use-package all-the-icons
  :ensure)

(setq default-frame-alist
      (append '((width                . 120)  ; フレーム幅
                (height               . 42 ) ; フレーム高
                ;; (left                 . 70 ) ; 配置左位置
                ;; (top                  . 28 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 12 ) ; 左フリンジ幅
                (right-fringe         . 12 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                ;; (tool-bar-lines       . 1  ) ; ツールバー
                ;;   (vertical-scroll-bars . 1  ) ; スクロールバー
                ;;   (scroll-bar-width     . 17 ) ; スクロールバー幅
                (cursor-type          . box) ; カーソル種別
                (alpha                . 100) ; 透明度
                )
              default-frame-alist))
(setq initial-frame-alist default-frame-alist)

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "*[^*]+*"))

;; 本当はひとつのuse-packageにまとめたいが、spaceline-define-segmentがマクロ展開できないため、先にrequireする
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-define-segment my/buffer-modified
    "A modified segment"
    (let* ((config-alist '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :v-adjust 0.0)
                           ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :v-adjust 0.0)
                           ("%" all-the-icons-faicon-family all-the-icons-faicon "lock" :v-adjust 0.0)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))
      (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit))))

  (spaceline-define-segment my/major-mode
    "A major-mode segment"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon)
        (propertize icon
                    'mouse-face 'mode-line-highlight
                    'help-echo (format "Major mode: `%s'\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes" major-mode)
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line down-mouse-1]
                                   `(menu-item ,(purecopy "Menu Bar") ignore
                                               :filter (lambda (_) (mouse-menu-major-mode-map))))
                                 (define-key map [mode-line mouse-2] 'describe-mode)
                                 (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                                 map)
                    'face `(:family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

  (spaceline-define-segment my/time
    "A datetime segment"
    (propertize (format-time-string "%m/%d %H:%M") 'display '(raise 0.1)
                'help-echo (format-time-string "%c"))
    :tight t :enabled t))

(use-package spaceline-config
  :config
  (set-face-attribute 'mode-line nil
                      :foreground nil
                      :background nil
                      :box nil
                      :height 120)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground nil
                      :background nil
                      :height 120)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground nil
                      :background nil
                      :box nil
                      :height 120)
  (set-face-attribute 'mode-line-buffer-id-inactive nil
                      :foreground nil
                      :background nil
                      :box nil
                      :height 120)
  (setq-default powerline-default-separator 'wave
                spaceline-separator-dir-left '(left . left)
                spaceline-separator-dir-right '(right . right)
                powerline-height 24
                powerline-text-scale-factor 0.78
                mode-line-format '("%e" (:eval (spaceline-ml-main))))
  (spaceline-helm-mode +1)
  (spaceline-compile
    `(((my/buffer-modified buffer-size))
      (anzu :when active)
      (buffer-id remote-host)
      my/major-mode
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active)
      (minor-modes :when active)
      ("" version-control :when active)
      (org-clock :when active))
    `((selection-info :when mark-active)
      ((buffer-encoding-abbrev point-position line-column))
      buffer-position
      my/time
      )))

(defun my/spaceline-all-the-icons--height (&optional height)
  "Scale `powerline-text-scale-factor' by HEIGHT."
  (if (bound-and-true-p powerline-text-scale-factor)
      (* (or height 1) (or powerline-text-scale-factor 1))
    (or height 1)))

(use-package popwin
  :ensure
  :defer t
  :custom
  (popwin:popup-window-position 'bottom))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 入力
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ddskk
  :ensure
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 検索
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package google-this
  :ensure
  :defer t
  :bind (("M-s g" . google-this-noconfirm)))

;; anzu
(use-package anzu
  :ensure
  :defer t
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
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs" "-i" "\a"))
  (migemo-dictionary (file-truename "~/../../usr/local/share/migemo/utf-8/migemo-dict"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init))

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

(use-package quickrun
  :ensure
  :commands (quickrun)
  :init
  (bind-key "C-c C-c" 'quickrun prog-mode-map))

(use-package indent-guide
  :ensure
  :defer t
  :diminish ""
  :config
  (defvar indent-guide-delay 0.1)
  (defvar indent-guide-recursive t)
  :hook (prog-mode . indent-guide-mode))

;; 操作した際に、操作箇所を強調表示する
(use-package volatile-highlights
  :ensure
  :diminish ""
  :config
  (volatile-highlights-mode t))

;; vi風に空行に~を表示する
(use-package vi-tilde-fringe
  :ensure
  :commands vi-tilde-fringe-mode
  :diminish ""
  :hook
  (prog-mode . vi-tilde-fringe-mode))

(use-package rainbow-mode
  :defer t
  :ensure)

(use-package neotree
  :defer t
  :ensure)

(use-package flycheck
  :ensure
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode))

(use-package eglot
  :ensure
  :defer t
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("go-langserver"
                                                   "-mode=stdio"
                                                   "-gocodecompletion"
                                                   "-diagnostics"
                                                   "-lint-tool=golint")))
  :hook
  (go-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (js2-mode . eglot-ensure))

(use-package go-mode
  :ensure
  :defer t
  :commands (gofmt-before-save)
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4))

(use-package web-mode
  :ensure
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'rainbow-mode)
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
  :ensure
  :defer t
  :commands (emmet-mode)
  :hook
  (web-mode . emmet-mode))

(use-package js2
  :ensure js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

(use-package coffee-mode
  :ensure
  :defer t
  :config
  (setq coffee-tab-width 2))

(use-package php-mode
  :ensure
  :mode ("\\.php\\'" . php-mode))

(use-package web-beautify
  :ensure
  :defer t)

(use-package groovy-mode
  :ensure
  :defer t
  :mode (("Jenkinsfile" . groovy-mode)))

(use-package rust-mode
  :ensure
  :defer t
  :config
  (setq-default rust-format-on-save t))

(use-package racer
  :ensure
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :ensure
  :defer t
  :after racer
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-rust-setup))))

(use-package alchemist
  :ensure
  :defer t
  :config
  (setq alchemist-hooks-compile-on-save t))

(use-package elixir-mode
  :ensure
  :defer t
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  )

(use-package flycheck-elixir
  :ensure
  :defer t)

(use-package elixir-yasnippets
  :ensure
  :defer t)

(use-package python-mode
  :ensure
  :defer t
  :mode (("\\.py\\'" . python-mode))
  :config
  (bind-key "C-c C-c" 'quickrun python-mode-map)
  )

(use-package ess
  :ensure
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  (setq ess-ask-for-ess-directory nil)
  (use-package ess-site)
  (use-package ess-R-object-popup
    :ensure
    :init
    (bind-key "C-c C-g" 'ess-R-object-popup ess-mode-map))
  (use-package ess-R-data-view
    :ensure))

(use-package yaml-mode
  :ensure
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package markdown-mode
  :ensure
  :mode ("\\.md\\'" . gfm-mode))


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

(use-package smartrep
  :defer t
  :ensure)

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
  :diminish ""
  :config
  (global-aggressive-indent-mode t))


(use-package smartparens
  :diminish smartparens-mode
  :ensure)
(use-package smartparens-config
  :after smartparens
  :config
  (smartparens-global-mode t))

(use-package smooth-scroll
  :ensure
  :diminish ""
  :config
  (smooth-scroll-mode t))

(use-package rainbow-delimiters
  :ensure
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package fontawesome
  :ensure
  :defer t)

(use-package codic
  :ensure
  :defer t)

(use-package pocket-reader
  :ensure
  :defer t)


(use-package company
  :ensure
  :diminish company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-begin-commands '(self-insert-command)
        company-selection-wrap-around t
        company-show-numbers t)
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora/company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(defun ora/company-number ()
  "Forward to `company-complete-number'."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))


;; avy
(use-package avy
  :ensure
  :bind
  (("C-:" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)))


(use-package view
  :ensure
  :defer t
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
                            "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME: %(format-time-string \"%Y%m%d%H%M%S\")\n:END:\n")
                           )))

;;; TODOの場合だけSTARTEDに変更する
(defun my-org-clock-in-switch-to-state (state)
  "Change state to STRTED when previous STATE is only TODO."
  (when (string-equal state "TODO")
    "STARTED"))

(use-package org-bullets
  :disabled t
  :ensure
  :hook
  (org-mode (lambda () (org-bullets-mode 1))))

;;; #+UPDATE:を保存時に更新
(use-package time-stamp
  :defer t
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
  :ensure
  :after ox)
(use-package ox-hugo-auto-export)

(use-package ob
  :defer t
  :config
  (use-package ob-elixir
    :ensure)
  (use-package ob-go
    :ensure)
  (use-package ob-rust
    :ensure)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)
     (go . t)
     (rust . t))))

(use-package magit
  :ensure
  :bind (("C-x g" . magit-status)))

;; gitの差分を表示する
(use-package git-gutter-fringe
  :ensure
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))

(use-package helm
  :diminish helm-mode
  :ensure
  :init
  (global-unset-key (kbd "C-z"))
  (use-package helm-config
    :init
    (setq helm-command-prefix-key "C-z"))
  (use-package helm-descbinds
    :ensure
    :bind
    (("C-z d" . helm-descbinds))
    :config
    (helm-descbinds-mode))
  (use-package helm-swoop
    :ensure
    :bind
    (("C-z w" . helm-swoop)))
  (use-package helm-ghq
    :ensure
    :bind
    (("C-z g" . helm-ghq)))
  (use-package helm-ag
    :ensure
    :bind
    (("C-z ;" . helm-ag))
    :config
    (setq helm-ag-base-command "rg -S --no-heading"))
  :bind
  (("C-;" . helm-mini)
   ("C-M-z" . helm-resume)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  :config
  (helm-mode t))


;; C-hをバックスペース
(keyboard-translate ?\C-h ?\C-?)

;; sequential-command
(use-package sequential-command-config
  :ensure sequential-command
  :defer t
  :bind (("C-a" . seq-home)
         ("C-e" . seq-end)))

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

(defun my/copy-now-line ()
  "現在ファイルと行をコピーする."
  (interactive)
  (let ((text (format "%s L%d" (buffer-name) (line-number-at-pos))))
    (message text)
    (kill-new text)))

;;; サスペンドさせない
(global-unset-key (kbd "C-x C-z"))




;;; init.el ends here
