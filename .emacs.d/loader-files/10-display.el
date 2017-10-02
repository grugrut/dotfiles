;; ツールバーを表示しない
(tool-bar-mode 0)

;; スクロールバーを表示しない
(set-scroll-bar-mode nil)

;; 行番号を表示
(line-number-mode +1)
(column-number-mode +1)

;; 現在行を強調
;(global-hl-line-mode)

(use-package beacon
  :ensure t
  :diminish ""
  :config
  (beacon-mode 1))

(use-package indent-guide
  :ensure t
  :diminish ""
  :init
  (defvar indent-guide-delay 0.1)
  (defvar indent-guide-recursive t)
  (add-hook 'prog-mode-hook 'indent-guide-mode))

;; タイトルバーにファイル名を表示
(setq frame-title-format "%f")

;; 対応する括弧を光らせる
(show-paren-mode t)
(defvar show-paren-style 'mixed)

;; カーソルを点滅させない
(blink-cursor-mode 0)

;; 空白文字を強調
(setq whitespace-style '(spaces tabs space-mark tab-mark))
(setq whitespace-display-mappings
      '(
        ;;; (space-mark 32 [183] [46]) ; normal space, ·
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
        (space-mark ?\x3000 [?\□]) ;;; Zenkaku space [　]
        ;(tab-mark ?\t [?\x276F ?\t] [?\\ ?\t]) ; tab, ❯ [ ]
        ))
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-tab nil
                    :foreground "LightSkyBlue"
                    :underline t)

;;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; 単語での折り返し
(global-visual-line-mode t)

;; マウスを避けさせる
(mouse-avoidance-mode 'jump)

;; デフォルト フォント
(set-face-attribute 'default nil :family "Migu 1M" :height 140)
;(set-face-font 'default "Migu 1M-11:antialias=standard")

;; プロポーショナル フォント
(set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 140)
;;(set-face-font 'variable-pitch "Migu 1M-11:antialias=standard")

;; 等幅フォント
(set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 140)
;;(set-face-font 'fixed-pitch "Migu 1M-11:antialias=standard")

;; ツールチップ表示フォント
(set-face-attribute 'tooltip nil :family "Migu 1M" :height 110)
;;(set-face-font 'tooltip "Migu 1M-9:antialias=standard")

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

;; バッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-lines nil)

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode t)
  (setq nlinum-format "%5d "))

;; 操作した際に、操作箇所を強調表示する
(use-package volatile-highlights
  :ensure t
  :diminish ""
  :init
  (volatile-highlights-mode t))

;; vi風に空行に~を表示する
(use-package vi-tilde-fringe :ensure t
  :diminish ""
  :init
  (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode))

(use-package rainbow-mode
  :ensure t)

(use-package neotree
  :ensure t)

(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom))

(use-package zenburn-theme
  :disabled t
  :ensure t
  :init
  (load-theme 'zenburn)
  :config
  (zenburn-with-color-variables
   (custom-theme-set-faces
    'zenburn
    `(region ((t (:background ,zenburn-bg-2)))))))


(use-package solarized-theme
  :disabled t
  :ensure t
  :init
  (load-theme 'solarized-dark))

(load-theme 'manoj-dark)
