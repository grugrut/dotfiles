;;; init.el --- My init script -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; パッケージ
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(straight-use-package 'leaf)
(leaf leaf
  :custom
  (leaf-enable-imenu-support . t))
(straight-use-package 'leaf-keywords)
(leaf leaf-keywords
  :config
  (leaf-keywords-init))

;;; diminishが付属しなくなったので手動で入れる
(leaf leaf-util-packages
  :config
  (leaf diminish :straight t :require t)
  (leaf bind-key)
  (leaf key-chord
    :straight (key-chord :host github :repo "zk-phi/key-chord" :branch "master")
    :require t
    :config
    (key-chord-mode t))
  (leaf hydra
    :straight t))

;; ベンチマーク
(leaf benchmark-init
  :straight t
  :config
  (benchmark-init/activate)
  :hook
  (after-init-hook . benchmark-init/deactivate))

;;; ライブラリ群
(leaf libraries
  :config
  (leaf cl-lib
    :leaf-defer t)
  (leaf dash
    :straight t
    :leaf-defer t)
  (leaf s
    :straight t
    :leaf-defer t)
  (leaf f
    :straight t
    :leaf-defer t)
  (leaf ht
    :straight t
    :leaf-defer t)
  (leaf posframe
    :straight t
    :leaf-defer t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 全般
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf general-setting
  :config
  ;; 色をつける
  (global-font-lock-mode t)

  ;; GC
  (setq gc-cons-threshold (* 256 1024 1024))
  (setq garbage-collection-messages t)
  
  ;; バッファの自動掃除
  (leaf midnight
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
  (setq-default indent-tabs-mode nil
                tab-width 2)

  ;; バックアップを作成しない
  (setq backup-inhibited t)
  ;; 初期画面を表示しない
  (setq inhibit-startup-message t)

  ;; ブラウザ設定 WSL限定
  (setq browse-url-browser-function 'browse-url-generic)
  (defvar browse-url-generic-program  (executable-find (getenv "BROWSER")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 画面
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-frame-alist
      '((width . 180)
        (height . 50)))

(setq initial-frame-alist
      '((width . 180)
        (height . 50)))


;; フォント設定
;;
;; abcdefghi
;; 012345678
;; あいうえお
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Migu 1M"))

(leaf text-scale
  :hydra (hydra-zoom ()
                     "Zoom"
                     ("g" text-scale-increase "in")
                     ("l" text-scale-decrease "out")
                     ("r" (text-scale-set 0) "reset")
                     ("0" (text-scale-set 0) :bind nil :exit t)
                     ("1" (text-scale-set 0) nil :bind nil :exit t))
  :bind ("<f2>" . hydra-zoom/body))

;; 絵文字
;; (unicode-fonts-setup) ; 最初に本コマンドの実行が必要
;; (all-the-icons-install-fonts)
(leaf unicode-fonts
  :straight t)
(leaf all-the-icons
  :straight t)

(add-to-list 'custom-theme-load-path "~/src/github.com/grugrut/doom-manoj-dark-theme.el/")

(leaf doom-themes
  :straight t
  :config
  (load-theme 'doom-manoj-dark t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(leaf minions
  :straight t
  :config
  (minions-mode t))

(leaf doom-modeline
  :straight t
  :require t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-height . 20)
  (doom-modeline-major-mode-color-icon . nil)
  (doom-modeline-minor-modes . t)
  (doom-modeline-github . nil))

;; ツールバーを表示しない
(tool-bar-mode 0)

;; スクロールバーを表示しない
(set-scroll-bar-mode nil)

;; 行番号を表示
(line-number-mode +1)
(column-number-mode +1)

(leaf beacon
  :straight t
  :diminish beacon-mode
  :require t
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
(leaf uniquify
  :custom
  (uniquify-buffer-name-style . 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re . "*[^*]+*"))

(leaf popwin
  :straight t
  :custom
  (popwin:popup-window-position . 'bottom))

(leaf treemacs
  :straight t
  :require t
  :bind (("H-t" . treemacs-select-window)
         ("H-T" . treemacs))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t))
(leaf treemacs-projectile
  :straight t
  :require t
  :after (treemacs projectile))
(leaf treemacs-magit
  :straight t
  :require t
  :after (treemacs magit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 移動
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf backward-forward
  :straight t
  :config
  (backward-forward-mode 1))

(leaf avy
  :straight t
  :bind
  (("C-:" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)))

(leaf ace-window
  :straight t
  :bind
  (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face . '((t (:height 2.0)))))

(leaf bm
  :straight t
  :leaf-defer t
  :commands (bm-toggle
             bm-next
             bm-previous)
  :bind
  (("C-S-SPC" . bm-toggle)
   ("C-}" . bm-previous)
   ("C-]" . bm-next)))

(leaf move-with-hydra
  :chord (("jk" . hydra-move/body))
  :hydra (hydra-move
          (:hint nil)
          "
          ^move^
-------------------------------------
_gg_
_G_
"
          ("gg" (goto-char (point-min)))
          ("G" (goto-line (point-max)))
          ("q" nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 入力・編集
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf ddskk
  :straight t
  :require t
  :bind
  (("C-x C-j" . skk-mode)
   ("C-x j"   . skk-mode))
  :init
  (defvar dired-bind-jump nil)  ; dired-xがC-xC-jを奪うので対処しておく
  :custom
  (skk-use-azik . t)                     ; AZIKを使用する
  (skk-azik-keyboard-type . 'jp106)      ;
  (skk-tut-file . nil)                   ;
  (skk-server-host . "localhost")        ;
  (skk-server-portnum . 1178)            ;
  (skk-egg-like-newline . t)             ; 変換時にリターンでは改行しない
  (skk-japanese-message-and-error . t)   ; メッセージを日本語にする
  (skk-auto-insert-paren . t)            ; 対応する括弧を自動挿入
  (skk-check-okurigana-on-touroku . t)   ;
  (skk-show-annotation . t)              ; アノテーションを表示
  (skk-anotation-show-wikipedia-url . t) ;
  (skk-show-tooltip . nil)               ; 変換候補をインライン表示しない
  (skk-isearch-start-mode . 'latin)      ; isearch時にSKKをオフ
  (skk-henkan-okuri-strictly . nil)      ; 送り仮名を考慮した変換候補
  (skk-process-okuri-early . nil))

;; 操作した際に、操作箇所を強調表示する
(leaf volatile-highlights
  :straight t
  :require t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; 最終行には必ず1行挿入
(setq require-final-newline t)

;; バッファの最後の改行を抑制
(setq next-line-add-newlines nil)

;; リージョン選択時にリージョンまるごと削除
(delete-selection-mode t)

(leaf highlight-symbol
  :straight t
  :leaf-defer t
  :bind
  (("C-." . highlight-symbol-at-point)))

(leaf expand-region
  :straight t
  :leaf-defer t
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

(leaf smartrep
  :straight t)

(leaf multiple-cursors
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

(leaf smooth-scroll
  :straight t
  :require t
  :diminish smooth-scroll-mode
  :config
  (smooth-scroll-mode t))

(leaf auto-revert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 検索
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf google-this
  :straight t
  :leaf-defer t
  :bind (("M-s g" . google-this-noconfirm)))

(leaf anzu
  :straight t
  :bind
  (("M-%" . anzu-query-replace))
  :config
  (global-anzu-mode +1)
  )

(leaf migemo
  :straight t
  :require t
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system . 'utf-8-unix)
  :config
  (migemo-init))

(leaf ripgrep
  :straight t
  :leaf-defer t
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

;; minibufferのアクティブ時、IMEを無効化
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (deactivate-input-method)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; コーディング
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq comment-style 'extra-line)

(leaf yafolding
  :straight t
  :leaf-defer t
  :hook
  (prog-mode-hook . yafolding-mode))

(leaf projectile
  :straight t t
  :init
  :config
  (setq projectile-mode-line-prefix " Prj")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(leaf quickrun
  :straight t
  :leaf-defer t
  :commands (quickrun)
  :init
  (bind-key "C-c C-c" 'quickrun prog-mode-map))

(leaf highlight-indent-guides
  :straight t
  :require t
  :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method . 'character)
  (highlight-indent-guides-auto-character-face-perc . 20)
  (highlight-indent-guides-character . ?\|)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

;; vi風に空行に~を表示する
(leaf vi-tilde-fringe
  :straight t
  :require t
  :leaf-defer t
  :commands vi-tilde-fringe-mode
  :diminish vi-tilde-fringe-mode
  :hook
  (prog-mode-hook . vi-tilde-fringe-mode))

(leaf aggressive-indent
  :straight t
  :require t
  :diminish aggressive-indent-mode
  :hook
  (prog-mode-hook . aggressive-indent-mode))

(leaf minimap
  :disabled t
  :straight t
  :leaf-defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0.2
        minimap-minimum-width 20)
  :hook
  (prog-mode-hook . minimap-mode))

(leaf rainbow-mode
  :straight t
  :leaf-defer t
  :hook
  (web-mode-hook . rainbow-mode))

(leaf neotree
  :straight t)

(leaf flycheck
  :straight t
  :leaf-defer t
  :diminish flycheck-mode
  :hook (prog-mode-hook . flycheck-mode))

;; (leaf eglot
;;   :straight t
;;   :leaf-defer t
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

(leaf lsp-mode
  :straight t
  :require t
  :commands lsp
  :hook
  (go-mode-hook . lsp)
  (web-mode-hook . lsp)
  (elixir-mode-hook . lsp))

(leaf lsp-ui
  :straight t
  :require t
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable . nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c i") 'lsp-ui-imenu)
  (define-key lsp-ui-mode-map (kbd "s-l") 'hydra-lsp/body)
  (setq lsp-ui-doc-position 'bottom)
  :hydra (hydra-lsp (:exit t :hint nil)
                    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
                    ("d" lsp-find-declaration)
                    ("D" lsp-ui-peek-find-definitions)
                    ("R" lsp-ui-peek-find-references)
                    ("i" lsp-ui-peek-find-implementation)
                    ("t" lsp-find-type-definition)
                    ("s" lsp-signature-help)
                    ("o" lsp-describe-thing-at-point)
                    ("r" lsp-rename)

                    ("f" lsp-format-buffer)
                    ("m" lsp-ui-imenu)
                    ("x" lsp-execute-code-action)

                    ("M-s" lsp-describe-session)
                    ("M-r" lsp-restart-workspace)
                    ("S" lsp-shutdown-workspace)))

(leaf company-lsp
  :straight t
  :require t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(leaf company-tabnine
  :straight t
  :after company
  :require t
  :config
  (add-to-list 'company-backends #'company-tabnine))
  

(leaf go-mode
  :straight t
  :leaf-defer t
  :commands (gofmt-before-save)
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4))

(leaf go-impl
  :straight t
  :leaf-defer t
  :commands go-impl)

(leaf web-mode
  :straight t
  :leaf-defer t
  :after flycheck
  :defun flycheck-add-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.js\\'" . web-mode))
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

(leaf emmet-mode
  :straight t
  :leaf-defer t
  :commands (emmet-mode)
  :hook
  (web-mode-hook . emmet-mode))

(leaf js2-mode
  :straight t
  :disabled t
  :leaf-defer t
  :mode ("\\.js\\'" . js2-mode)
  :bind (:js2-mode-map
         ("C-c b" . web-beautify-js))
  )

(leaf coffee-mode
  :straight t
  :leaf-defer t
  :config
  (setq coffee-tab-width 2))

(leaf php-mode
  :straight t
  :leaf-defer t
  :mode ("\\.php\\'" . php-mode))

(leaf web-beautify
  :straight t
  :leaf-defer t)

(leaf groovy-mode
  :straight t
  :leaf-defer t
  :mode (("Jenkinsfile" . groovy-mode)))

(leaf rust-mode
  :straight t
  :leaf-defer t
  :config
  (setq-default rust-format-on-save t))

(leaf racer
  :straight t
  :leaf-defer t
  :hook
  (rust-mode-hook . racer-mode)
  (racer-mode-hook . eldoc-mode))

(leaf flycheck-rust
  :straight t
  :leaf-defer t
  :after racer
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-rust-setup))))

(leaf alchemist
  :straight t
  :leaf-defer t
  :config
  (setq alchemist-hooks-compile-on-save t))

(defun my/elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(leaf elixir-mode
  :straight t
  :after smartparens
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "->" "end"
                   :when '(("RET"))
                   :post-handlers '(:add my/elixir-do-end-close-action)
                   :actions '(insert)))
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(:add my/elixir-do-end-close-action)
                   :actions '(insert)))
  )

(leaf flycheck-elixir
  :straight t
  :leaf-defer t
  :after elixir-mode)

(leaf elixir-yasnippets
  :straight t
  :leaf-defer t
  :after elixir-mode)

(leaf python-mode
  :straight t
  :leaf-defer t
  :mode (("\\.py\\'" . python-mode))
  :config
  (bind-key "C-c C-c" 'quickrun python-mode-map)
  )

(leaf yaml-mode
  :straight t
  :leaf-defer t
  :mode ("\\.yaml\\'" . yaml-mode))

(leaf markdown-mode
  :straight t
  :leaf-defer t
  :mode ("\\.md\\'" . gfm-mode))

(leaf smartparens
  :straight t
  :require t
  :diminish smartparens-mode
  :config
  (leaf smartparens-config
    :require t
    :after smartparens
    :hook
    (prog-mode-hook . smartparens-mode)))

(leaf rainbow-delimiters
  :straight t
  :leaf-defer t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf fontawesome
  :straight t)

(leaf codic
  :straight t
  :leaf-defer t)

(leaf pocket-reader
  :straight t
  :leaf-defer t)

(leaf company
  :straight t
  :require t
  :diminish company-mode
  :defun (global-company-mode
              company-abort
              company-complete-number)
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-begin-commands '(self-insert-command)
        company-selection-wrap-around t
        company-show-numbers t))

(leaf company-box
  :straight t
  :require t
  :diminish company-box-mode
  :hook (company-mode-hook . company-box-mode)
  :init
  (setq company-box-icons-elisp
        (list
         (concat (all-the-icons-material "functions") " ")
         (concat (all-the-icons-material "check_circle") " ")
         (concat (all-the-icons-material "stars") " ")
         (concat (all-the-icons-material "format_paint") " ")))
  (setq company-box-icons-unknown (concat (all-the-icons-material "find_in_page") " "))
  (setq company-box-backends-colors nil)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(leaf company-posframe
  :straight t
  :require t
  :diminish company-posframe-mode
  :after company
  :config
  (company-posframe-mode 1))

(leaf yasnippet
  :straight t
  :diminish yas-minor-mode
  :require t
  :defun yas-global-mode
  :config
  (yas-global-mode 1))

(leaf view
  :require t
  :chord (("fj" . view-mode))
  :bind  (:view-mode-map
          ("j" . next-line)
          ("k" . previous-line)
          ("h" . backward-char)
          ("l" . forward-char))
  :config
  (setq view-read-only t))

(leaf org
  :leaf-defer t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :mode ("\\.org$'" . org-mode)
  ;; :hook  (org-mode . (lambda ()
  ;;                      (set (make-local-variable 'system-time-locale) "C")))
  :custom
  (org-directory . "~/org/")
  ;; TODO状態の設定
  (org-todo-keywords . '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
  (org-todo-keyword-faces . '(("TODO" :foreground "red" :weight bold)
                              ("STARTED" :foreground "cornflower blue" :weight bold)
                              ("DONE" :foreground "green" :weight bold)
                              ("WAITING" :foreground "orange" :weight bold)
                              ("HOLD" :foreground "magenta" :weight bold)
                              ("CANCELLED" :foreground "green" :weight bold)
                              ("MEETING" :foreground "gren" :weight bold)))
  ;; 時間計測を開始したらSTARTED状態に
  (org-clock-in-switch-to-state . 'my-org-clock-in-switch-to-state)
  (org-log-done . 'time)                
  (org-clock-persist . t)
  (org-clock-out-when-done . t)
  )

(leaf org-capture
  :leaf-defer t
  :after org
  :commands (org-capture)
  :config
  (setq org-capture-templates `(
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

(leaf org-bullets
  :disabled t
  :straight t
  :hook
  (org-mode-hook (lambda () (org-bullets-mode 1))))

;;; #+UPDATE:を保存時に更新
(leaf time-stamp
  :disabled t
  :init
  (add-hook 'before-save-hook 'time-stamp)
  :config
  (setq time-stamp-active t
        time-stamp-line 10
        time-stamp-start "^#\\+LASTMOD:"
        time-stamp-format " %:y-%02m-%02d"
        time-stamp-end "$"))

(leaf ox-hugo
  :straight t
  :after ox
  :mode ("\\.org$'" . org-hugo-auto-export-mode))

(leaf ob
  :leaf-defer t
  :after org
  :defun org-babel-do-load-languages
  :config
  (leaf ob-elixir
    :straight t)
  (leaf ob-go
    :straight t)
  (leaf ob-rust
    :straight t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)
     (go . t)
     (rust . t))))

(leaf magit
  :straight t
  :bind (("C-x g" . magit-status)))

;; gitの差分を表示する
(leaf git-gutter-fringe
  :straight t
  :require t
  :custom
  (git-gutter:lighter . "")
  (global-git-gutter-mode . t)
  :bind ("C-x C-g" . hydra-git-gutter/body)
  :hydra (hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
                           "
Git gutter:
  _j_: next hunk     _s_tage hunk   _q_uit
  _k_: previous hunk _r_evert hunk
  _h_: first hunk    _p_opup hunk
  _l_: last hunk     set _R_evision
"
                           ("j" git-gutter:next-hunk)
                           ("k" git-gutter:previous-hunk)
                           ("h" (progn (goto-char (point-min))
                                       (git-gutter:next-hunk 1)))
                           ("l" (progn (goto-char (point-min))
                                       (git-gutter:previous-hunk 1)))
                           ("s" git-gutter:stage-hunk)
                           ("r" git-gutter:revert-hunk)
                           ("p" git-gutter:popup-hunk)
                           ("R" git-gutter:set-start-revision)
                           ("q" nil :color blue)))

(leaf helm
  :diminish helm-mode
  :require t
  :straight t
  :init
  (global-unset-key (kbd "C-z"))
  (leaf helm-config
    :require t
    :init
    (setq helm-command-prefix-key "C-z"))
  (leaf helm-descbinds
    :straight t
    :bind
    (("C-z d" . helm-descbinds))
    :config
    (helm-descbinds-mode))
  (leaf helm-swoop
    :straight t
    :bind
    (("C-z w" . helm-swoop)))
  (leaf helm-ghq
    :straight t
    :bind
    (("C-z g" . helm-ghq)))
  (leaf helm-ag
    :straight t
    :bind
    (("C-z ;" . helm-ag))
    :config
    (setq helm-ag-base-command "rg -S --no-heading"))
  (leaf helm-projectile
    :straight t
    :config
    (helm-projectile-on))
  (leaf helm-git-grep
    :straight t
    :bind
    (("C-z s" . helm-git-grep)))
  :bind
  (("C-;" . helm-mini)
   ("C-M-z" . helm-resume)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   (helm-map
    ("C-z" . helm-execute-persistent-action)))
  :config
  (helm-mode t))

(leaf helm-posframe
  :straight t
  :disabled t
  :config
  (helm-posframe-enable)
  :custom
  (helm-posframe-poshandler . 'posframe-poshandler-frame-center))

;; C-hをバックスペース
(keyboard-translate ?\C-h ?\C-?)

;; which-key
(leaf which-key
  :straight t
  :require t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))
(leaf which-key-posframe
  :straight t
  :disabled t
  :config
  (which-key-posframe-mode)
  :custom
  (which-key-posframe-border-width . 2))

(leaf keyfreq
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
