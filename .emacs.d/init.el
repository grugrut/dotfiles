;;; init.el --- My init script -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

;; leaf.el

(prog1 "leaf"
  (prog1 "install leaf"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf)))

  (leaf leaf-keywords
    :ensure t
    :config
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone . t)))

    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf-util-packages
  :config
  (leaf diminish :ensure t :require t)
  (leaf bind-key)
  (leaf key-chord
    :el-get (key-chord
             :url "https://raw.githubusercontent.com/zk-phi/key-chord/master/key-chord.el")
    :require t
    :config (key-chord-mode 1)))

(leaf paradox
  :ensure t
  :config
  (paradox-enable))

(leaf early-init
  :doc "emacs26以前はearly-init.elが使えないので手動で読みこむ"
  :emacs< "27.1"
  :config
  (load (concat user-emacs-directory "early-init.el"))
  )

(leaf libraries
  :doc "ライブラリ群"
  :config
  (leaf cl-lib
    :leaf-defer t)
  (leaf dash
    :ensure t
    :leaf-defer t)
  (leaf posframe
    :ensure t
    :preface
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 1 :internal-border-color "gray80")))
        (or (plist-get info arg-name) value)))
    :custom
    (posframe-arghandler . #'my-posframe-arghandler)
    :leaf-defer t)
  (leaf smartrep
    :ensure t
    :leaf-defer t))

(leaf gcmh
  :ensure t
  :diminish gcmh
  :custom
  (gcmh-verbose . t)
  :config
  (gcmh-mode 1))

(defun grugrut/gc-debug-function (str)
  (let ((sum 0))
    (dolist (x str)
      (setq sum (+ sum (* (cl-second x) (cl-third x)))))
    (message "Used Memory: %d MB" (/ sum (* 1024 1024)))))
(advice-add 'garbage-collect :filter-return #'grugrut/gc-debug-function)

(leaf popwin
  :ensure t
  :custom
  (popwin:popup-window-position . 'bottom))

(leaf general-setting
  :config
  (prefer-coding-system 'utf-8-unix)
  (defalias 'yes-or-no-p 'y-or-n-p) ; yes-or-no-pをy/nで選択できるようにする
  ;; recentf
  (defvar recentf-max-saved-items 1000)
  (defvar recentf-auto-cleanup 'never)
  (global-set-key [mouse-2] 'mouse-yank-at-click)
  (delete-selection-mode t) ; リージョン選択時にリージョンまるごと削除
  (leaf exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  (leaf web-browser-for-wsl
    :doc "ブラウザ設定 WSL限定"
    :config
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
          (cmd-args '("/c" "start")))
      (when (file-exists-p cmd-exe)
        (setq browse-url-browser-function 'browse-url-generic
              browse-url-generic-program cmd-exe
              browse-url-generic-args cmd-args))))
  ;; 対応する括弧を光らせる
  (show-paren-mode t)
  (defvar show-paren-style 'mixed)
  ;; カーソルを点滅させない
  (blink-cursor-mode 0)
  ;; 単語での折り返し
  (leaf visual-line-mode
    :require simple
    :config
    (global-visual-line-mode t)
    (diminish 'visual-line-mode nil))

  ;; マウスを避けさせる
  (mouse-avoidance-mode 'jump)
  (setq frame-title-format "%f")
  :setq
  `((large-file-warning-threshold	         . ,(* 25 1024 1024))
    (read-file-name-completion-ignore-case . t)
    (use-dialog-box                        . nil)
    (history-length                        . 500)
    (history-delete-duplicates             . t)
    (line-move-visual                      . nil)
    (mouse-drag-copy-region                . t)
    (backup-inhibited                      . t)
    (inhibit-startup-message               . t)
    (require-final-newline                 . t)
    (next-line-add-newlines                . nil)
    (frame-title-format                    . "%f")
    (truncate-lines                        . t)
    (read-process-output-max               . ,(* 1024 1024)))
  :setq-default
  (indent-tabs-mode . nil) ; タブはスペースで
  (tab-width        . 2)
  )

;; 同一バッファ名にディレクトリ付与
(leaf uniquify
  :custom
  (uniquify-buffer-name-style . 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re . "*[^*]+*"))

(leaf font
  :config
  ;; 絵文字
  ;; (unicode-fonts-setup) ; 最初に本コマンドの実行が必要
  ;; (all-the-icons-install-fonts)
  (leaf unicode-fonts
    :ensure t)
  (leaf all-the-icons
    :ensure t)
  ;; フォント設定
  ;;
  ;; abcdefghik
  ;; 0123456789
  ;; あいうえお
  (let* ((family "Cica")
         (fontspec (font-spec :family family :weight 'normal)))
    (set-face-attribute 'default nil :family family :height 120)
    (set-fontset-font nil 'ascii fontspec nil 'append)
    (set-fontset-font nil 'japanese-jisx0208 fontspec nil 'append))
  (add-to-list 'face-font-rescale-alist '(".*icons.*" . 0.9))
  (add-to-list 'face-font-rescale-alist '(".*FontAwesome.*" . 0.9))
  (leaf text-scale
    :hydra (hydra-zoom ()
                       "Zoom"
                       ("g" text-scale-increase "in")
                       ("l" text-scale-decrease "out")
                       ("r" (text-scale-set 0) "reset")
                       ("0" (text-scale-set 0) :bind nil :exit t))
    :bind ("<f2>" . hydra-zoom/body)))

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf minions
  :ensure t
  :disabled t
  :config
  (minions-mode t))

(leaf eldoc
  :diminish eldoc-mode)

(leaf doom-modeline
  :ensure t
  :require t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 3)
  (doom-modeline-height . 25)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-minor-modes . t)
  (doom-modeline-github . nil)
  (doom-modeline-mu4e . nil)
  (doom-modeline-irc . nil))

(leaf beacon
  :ensure t
  :diminish beacon-mode
  :require t
  :config
  (beacon-mode 1))

;; 操作した際に、操作箇所を強調表示する
(leaf volatile-highlights
  :ensure t
  :require t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(leaf highlight-indent-guides
  :ensure t
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
  :ensure t
  :require t
  :leaf-defer t
  :commands vi-tilde-fringe-mode
  :diminish vi-tilde-fringe-mode
  :config
  (global-vi-tilde-fringe-mode))

(leaf minimap
  :ensure t
  :leaf-defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0.2
        minimap-minimum-width 20)
  :bind ("s-m" . minimap-mode))

(leaf rainbow-mode
  :ensure t
  :leaf-defer t
  :hook
  (web-mode-hook . rainbow-mode))

(leaf backward-forward
  :ensure t
  :config
  (backward-forward-mode 1))

(leaf bm
  :ensure t
  :leaf-defer t
  :commands (bm-toggle
             bm-next
             bm-previous)
  :bind
  (("C-S-SPC" . bm-toggle)
   ("C-}" . bm-previous)
   ("C-]" . bm-next)))

(leaf avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)
   ("C-*" . avy-resume)
   ("M-g M-g" . avy-goto-line))
  :config
  (leaf avy-zap
    :ensure t
    :bind
    ([remap zap-to-char] . avy-zap-to-char)))

(leaf ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face . '((t (:height 2.0)))))

(leaf ddskk
  :ensure t
  :bind
  (("C-x C-j" . skk-mode)
   ("C-x j"   . skk-mode))
  :init
  (defvar dired-bind-jump nil)  ; dired-xがC-xC-jを奪うので対処しておく
  :custom
  (skk-use-azik                     . t)      ; AZIKを使用する
  (skk-azik-keyboard-type           . 'jp106)
  (skk-tut-file                     . nil)
  (skk-server-host                  . "localhost")
  (skk-server-portnum               . 1178)   ;
  (skk-egg-like-newline             . t)      ; 変換時にリターンでは改行しない
  (skk-japanese-message-and-error   . t)      ; メッセージを日本語にする
  (skk-auto-insert-paren            . t)      ; 対応する括弧を自動挿入
  (skk-check-okurigana-on-touroku   . t)      ;
  (skk-show-annotation              . t)      ; アノテーションを表示
  (skk-anotation-show-wikipedia-url . t)      ;
  (skk-show-tooltip                 . nil)    ; 変換候補をインライン表示しない
  (skk-isearch-start-mode           . 'latin) ; isearch時にSKKをオフ
  (skk-henkan-okuri-strictly        . nil)    ; 送り仮名を考慮した変換候補
  (skk-process-okuri-early          . nil)
  (skk-status-indicator             . 'minor-mode)
  :hook
  (skk-azik-load-hook . my/skk-azik-disable-tU)
  :preface
  (defun my/skk-azik-disable-tU ()
    "ddskkのazikモードが`tU'を`っ'として扱うのを抑制する."
    (setq skk-rule-tree (skk-compile-rule-list
                         skk-rom-kana-base-rule-list
                         (skk-del-alist "tU" skk-rom-kana-rule-list)))))

(leaf highlight-symbol
  :ensure t
  :leaf-defer t
  :bind
  (("C-." . highlight-symbol-at-point)))

(leaf expand-region
  :ensure t
  :leaf-defer t
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

(leaf multiple-cursors
  :ensure t
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
  :ensure t
  :require t
  :diminish smooth-scroll-mode
  :config
  (smooth-scroll-mode t))

(leaf auto-revert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

(leaf search-functions
  :setq
  (case-fold-search . nil) ; 大文字・小文字を区別しないでサーチ（有効：t、無効：nil）
  (isearch-allow-scroll . nil) ; インクリメント検索時に縦スクロールを有効化（有効：t、無効：nil）
  :config
  (leaf google-this
    :ensure t
    :leaf-defer t
    :bind (("M-s g" . google-this-noconfirm)))

  (leaf anzu
    :ensure t
    :bind
    (("M-%" . anzu-query-replace))
    :config
    (global-anzu-mode +1)
    )

  (leaf migemo
    :ensure t
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
    :ensure t
    :leaf-defer t
    :bind (("M-s r" . ripgrep-regexp))
    :config
    (setq ripgrep-arguments '("-S")))
  ;; minibufferのアクティブ時、IMEを無効化
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (deactivate-input-method)))
  )

(setq comment-style 'extra-line)

(leaf imenu-list
  :ensure t
  :bind (("s-i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation . t)
  :config
  (leaf leaf-tree
    :doc "leafのブロックを意識して表示"
    :diminish leaf-tree
    :ensure t))

(leaf yafolding
  :ensure t
  :leaf-defer t
  :hook
  (prog-mode-hook . yafolding-mode))

(leaf projectile
  :ensure t t
  :init
  :config
  (setq projectile-mode-line-prefix " Prj")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(leaf quickrun
  :ensure t
  :leaf-defer t
  :after bind-key
  :commands (quickrun)
  :init
  (bind-key "C-c C-c" 'quickrun prog-mode-map))

(leaf neotree
  :ensure t
  :bind ("H-t" . neotree-toggle))

(leaf flycheck
  :ensure t
  :leaf-defer t
  :diminish flycheck-mode
  :hook (prog-mode-hook . flycheck-mode))

(leaf lsp-mode
  :ensure t
  :require t
  :commands lsp
  :hook
  (go-mode-hook . lsp)
  (web-mode-hook . lsp)
  (elixir-mode-hook . lsp)
  (typescript-mode-hook . lsp)
  :config
  (leaf lsp-ui
    :ensure t
    :require t
    :hook
    (lsp-mode-hook . lsp-ui-mode)
    :custom
    (lsp-ui-sideline-enable . nil)
    (lsp-prefer-flymake . nil)
    (lsp-print-performance . t)
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
                      ("S" lsp-shutdown-workspace))))

(leaf golang
  :config
  (leaf go-mode
    :ensure t
    :leaf-defer t
    :commands (gofmt-before-save)
    :init
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq tab-width 4))

  (leaf protobuf-mode
    :ensure t)

  (leaf go-impl
    :ensure t
    :leaf-defer t
    :commands go-impl))

(leaf web-mode
  :ensure t
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
  )

(leaf emmet-mode
  :ensure t
  :leaf-defer t
  :commands (emmet-mode)
  :hook
  (web-mode-hook . emmet-mode))

(leaf typescript-mode
  :ensure t)

(leaf groovy-mode
  :ensure t
  :leaf-defer t
  :mode (("Jenkinsfile" . groovy-mode)))

(leaf rust-mode
  :ensure t
  :leaf-defer t
  :config
  (setq-default rust-format-on-save t))

(leaf racer
  :ensure t
  :leaf-defer t
  :hook
  (rust-mode-hook . racer-mode)
  (racer-mode-hook . eldoc-mode))

(leaf flycheck-rust
  :ensure t
  :leaf-defer t
  :after racer
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-rust-setup))))

(leaf alchemist
  :ensure t
  :leaf-defer t
  :config
  (setq alchemist-hooks-compile-on-save t))

(defun my/elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(leaf elixir-mode
  :ensure t
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
  :ensure t
  :leaf-defer t
  :after elixir-mode)

(leaf elixir-yasnippets
  :ensure t
  :leaf-defer t
  :after elixir-mode)

(leaf python-mode
  :ensure t
  :leaf-defer t
  :custom ((python-shell-interpreter . "ipython"))
  :mode (("\\.py\\'" . python-mode))
  )

(leaf yaml-mode
  :ensure t
  :leaf-defer t
  :mode ("\\.yaml\\'" . yaml-mode))

(leaf markdown
  :config
  (leaf markdown-mode
    :ensure t
    :leaf-defer t
    :mode ("\\.md\\'" . gfm-mode)
    :custom
    (markdown-command . "github-markup")
    (markdown-command-needs-filename . t))
  (leaf markdown-preview-mode
    :ensure t))

(leaf dockerfile-mode
  :ensure t)

(leaf plantuml-mode
  :ensure t
  :mode ("\\.puml\\'" . plantuml-mode)
  :custom
  (plantuml-default-exec-mode . 'jar)
  (plantuml-jar-path . "~/bin/plantuml.jar")
  )

(leaf smartparens
  :ensure t
  :require smartparens-config
  :diminish smartparens-mode
  :hook
  (prog-mode-hook . turn-on-smartparens-mode)
  :config
  (show-smartparens-global-mode t))

(leaf rainbow-delimiters
  :ensure t
  :leaf-defer t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf fontawesome
  :ensure t)

(leaf codic
  :ensure t
  :leaf-defer t)

(leaf code-completion
  :config
  (leaf company
    :ensure t
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
    :ensure t
    :require t
    :diminish company-box-mode
    :hook (company-mode-hook . company-box-mode)
    :after all-the-icons
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
    :ensure t
    :require t
    :diminish company-posframe-mode
    :after company
    :config
    (company-posframe-mode 1))

  (leaf company-tabnine
    :ensure t
    :after company
    :require t
    :config
    (add-to-list 'company-backends #'company-tabnine)))

(leaf yasnippet
  :ensure t
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
         ("C-c a" . org-agenda)
         (:org-mode-map
          ("C-c C-;" . org-edit-special))
         (:org-src-mode-map
          ("C-c C-;" . org-edit-src-exit)))
  :mode ("\\.org$'" . org-mode)
  ;; :hook  (org-mode . (lambda ()
  ;;                      (set (make-local-variable 'system-time-locale) "C")))
  :config
  (setq org-directory "~/src/github.com/grugrut/PersonalProject/")
  :custom
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
  (org-log-done . 'time)
  (org-clock-persist . t)
  (org-clock-out-when-done . t)
  )
(leaf org-capture
  :leaf-defer t
  :after org
  :commands (org-capture)
  :config
  (defvar grugrut/org-inbox-file (concat org-directory "inbox.org"))
  (defvar grugrut/org-journal-file (concat org-directory "journal.org"))
  (setq org-capture-templates `(
                                ("t" " Tasks" entry (file ,grugrut/org-inbox-file)
                                 "* TODO %? %^G\n:PROPERTIES:\n:DEADLINE: %^{Deadline}T\n:EFFORT: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n")
                                ("e" " Event" entry (file ,grugrut/org-inbox-file)
                                 "* TODO %? %^G\n:PROPERTIES:\n:SCHEDULED: %^{Scheduled}T\n:EFFORT:%^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n")
                                ("j" " Journal" entry (file+olp+datetree ,grugrut/org-journal-file)
                                 "* %<%H:%M> %?")
                                ("b" " blog" entry
                                 (file+headline "~/src/github.com/grugrut/blog/draft/blog.org" ,(format-time-string "%Y"))
                                 "** TODO %?\n:PROPERTIES:\n:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :archives '(\\\"%(format-time-string \"%Y\")\\\" \\\"%(format-time-string \"%Y-%m\")\\\")\n:EXPORT_FILE_NAME: %(format-time-string \"%Y%m%d%H%M\")\n:END:\n\n")
                                )))

(leaf org-superstar
  :ensure t
  :custom
  (org-superstar-headline-bullets-list . '("󿕸" "󿖀" "󿕾" "󿕼" "󿕺" "󿖍"))
  :hook
  (org-mode-hook (lambda () (org-superstar-mode 1)))
  )

(leaf ox-hugo
  :ensure t
  :after ox
  :mode ("\\.org$'" . org-hugo-auto-export-mode))

(leaf ob
  :leaf-defer t
  :after org
  :defun org-babel-do-load-languages
  :config
  (setq org-plantuml-jar-path "~/bin/plantuml.jar")
  (leaf ob-elixir
    :ensure t)
  (leaf ob-go
    :ensure t)
  (leaf ob-rust
    :ensure t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)
     (go . t)
     (rust . t)
     (plantuml . t))))

(leaf git
  :config
  (leaf magit
    :ensure t
    :bind (("C-x g" . magit-status)))

  ;; gitの差分を表示する
  (leaf git-gutter-fringe
    :ensure t
    :require t
    :custom
    (git-gutter:lighter . "")
    (global-git-gutter-mode . t)
    :bind ("C-x G" . hydra-git-gutter/body)
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
                             ("q" nil :color blue))))

(leaf counsel
  :ensure t
  :require t
  :config
  (leaf ivy-hydra :ensure t)
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers . t)
  (ivy-wrap . t)
  (ivy-height . 15)
  (ivy-count-format . "(%d/%d) ")
  (ivy-truncate-lines . nil)
  (ivy-initial-inputs-alist . '())
  (ivy-format-functions-alist . '((t . grugrut/ivy-format-function)))
  (ivy-re-builders-alist . '((t . ivy--regex-ignore-order)))
  :bind
  (("C-z" . nil)
   ("C-;" . ivy-switch-buffer)
   ("C-+" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-z w" . swiper-all-thing-at-point)
   ("C-z s" . counsel-git-grep)
   ("C-z d" . counsel-descbinds)
   ("C-z i" . counsel-imenu)
   (ivy-minibuffer-map
    ("C-z" . grugrut/ivy-partial))
   (counsel-find-file-map
    ("C-l" . counsel-up-directory)))
  :preface
  (defun grugrut/ivy-partial ()
    "helmの `helm-execute-persistent-action' に近いものを実現する.
完全に同じものは無理だったので、ディレクトリなら入る、それ以外はできるだけ補完しバッファは抜けない動作をおこなう."
    (interactive)
    (cond
     ((eq (ivy-state-collection ivy-last) #'read-file-name-internal)
      ;; ファイルオープン
      (let (dir)
        (cond
         ((setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last)))
          ;; ディレクトリなら入る
          (ivy--cd dir))
         (t
          ;; それ以外ならチラ見アクション
          (ivy-call)))))
     (t
      (ivy-call))))
  (defun grugrut/ivy-format-function (cands)
    "選択の行頭にアイコンを表示する."
    (ivy--format-function-generic
     (lambda (str)
       (concat (all-the-icons-faicon "usb") " " (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "    " str))
     cands
     "\n"))
  )

(leaf all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(leaf ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(leaf ivy-posframe
  :ensure t
  :diminish t
  :custom
  (ivy-posframe-display-functions-alist . '((t . ivy-posframe-display-at-frame-center)))
  :init (ivy-posframe-mode 1))

(leaf atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server))

(defun grugrut/export-my-init-to-blog ()
  ""
  (interactive)
  (require 'ox-hugo)
  (let ((file "~/src/github.com/grugrut/blog/content/posts/my-emacs-init-el.md"))
    (org-hugo-export-as-md)
    (write-file file t)))

(leaf win-toast
  :el-get (win-toast
           :url "https://raw.githubusercontent.com/grugrut/win-toast/master/win-toast.el"))

(leaf key-settings
  :doc "キー入力設定"
  :config
  (global-unset-key (kbd "C-x C-z"))
  ;; C-hをバックスペース
  (keyboard-translate ?\C-h ?\C-?)
  ;; which-key
  (leaf which-key
    :ensure t
    :require t
    :diminish which-key-mode
    :config
    (which-key-mode)
    (which-key-setup-side-window-right-bottom))
  (leaf which-key-posframe
    :ensure t
    :after '(which-key posframe)
    :config
    (which-key-posframe-mode)
    :custom
    (which-key-posframe-border-width . 2))
  (leaf keyfreq
    :ensure t
    :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))
  (leaf free-keys
    :doc "利用していないキーマップを教えてくれる"
    :ensure t)
  )

;;; init.el ends here
