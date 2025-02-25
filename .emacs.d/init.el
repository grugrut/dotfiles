;;; init.el --- My init script -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

;; leaf.el
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords
    :ensure t
    :config
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf gcmh
  :ensure t
  :global-minor-mode t
  :custom
  (gcmh-verbose . t)
  )

(leaf general-settings
  :config
  (prefer-coding-system 'utf-8-unix)
  (global-set-key [mouse-2] 'mouse-yank-at-click)
  (global-unset-key "\C-z")
  (delete-selection-mode t)
  :setq
  (read-answer-short . t)
  ;(large-file-warning-threshold . '(* 25 1024 1024))
  (create-lockfiles . nil)
  (history-length . 500)
  (history-delete-duplicates . t)
  (line-move-visual . nil)
  (mouse-drag-copy-region . t)
  (backup-inhibited . t)
  (require-final-newline . t)
  )

(leaf :font
  :config
  ;; 絵文字インストール
  ;; (nerd-icons-install-fonts)
  (leaf nerd-icons
    :ensure t)
  ;; フォント設定
  ;; abcdefghik
  ;; 0123456789
  ;; あいうえお
  ;; 壱弐参四五
  (let* ((family "Cica")
         (fontspec (font-spec :family family :weight 'normal)))
    (set-face-attribute 'default nil :family family :height 120)
    (set-fontset-font nil 'ascii fontspec nil 'append)
    (set-fontset-font nil 'japanese-jisx0208 fontspec nil 'append)))

(leaf doom-themes
  :ensure t
  :defun (doom-themes-visual-bell-config)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf doom-modeline
  :ensure t
  :global-minor-mode t
  :custom
  (doom-modeline-bar-width . 4)
  (doom-modeline-hud . t))

(leaf shackle
  :ensure t
  :global-minor-mode t
  :custom
  (shackle-rules . '(("*Backtrace*" :popup t)
		     ("*Leaf Expand*" :popup t)
		     ("*Shell Command Output*" :popup t)
		     ))
  )

(leaf winner
  :global-minor-mode t
  :bind
  ("C-z" . winner-undo))

(leaf beacon
  :ensure t
  :global-minor-mode t)

(leaf volatile-highlights
  :ensure t
  :global-minor-mode t)

(leaf anzu
  :ensure t
  :global-minor-mode global-anzu-mode
  :bind
  (("M-%" . anzu-query-replace))
  )

(leaf migemo
  :ensure t
  :require t
  :defun
  (migemo-init)
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system . 'utf-8-unix)
  :config
  (migemo-init))

(leaf ddskk
  :ensure t
  :bind
  (("C-x C-j" . skk-mode)
   ("C-x j"   . skk-mode))
  :init
  (defvar dired-bind-jump nil) ; dired-xに `C-x C-j` が奪われてしまうので対処
  :custom
  (skk-use-azik                      . t) ; AZIKを使用
  (skk-azik-keyboard-type            . 'jp106)
  (skk-server-host                   . "localhost")
  (skk-server-portnum                . 1178)
  (skk-egg-like-newline              . t) ; 変換時にはリータンで改行しない
  (skk-japanese-message-and-error    . t)
  (skk-auto-insert-paren             . t)
  (skk-check-okurigata-on-touroku    . t)
  (skk-show-annotation               . t)
  (skk-annotation-show-wikipedia-url . t)
  (skk-show-tooltip                  . nil)
  (skk-isearch-start-mode            . 'latin)
  (skk-henkan-okuri-strictly         . nil)
  (skk-process-okuri-early           . nil)
  (skk-status-indicator              . 'minior-mode))

(leaf puni
  :ensure t
  :global-minor-mode puni-global-mode
  )

(leaf vertico
  :ensure t
  :global-minor-mode t
  :bind
  ((:vertico-map
    ("C-z" . vertico-insert)
    ("C-l" . grugrut/up-dir)))
  :preface
  (defun grugrut/up-dir ()
    "ひとつ上のディレクトリ階層に移動する."
    (interactive)
    (let* ((orig (minibuffer-contents))
           (orig-dir (file-name-directory orig))
           (up-dir (if orig-dir (file-name-directory (directory-file-name orig-dir))))
           (target (if (and up-dir orig-dir) up-dir orig)))
      (delete-minibuffer-contents)
      (insert target)))
  :custom
  (vertico-count . 20)
  (vertico-cycle . t))

(leaf savehist
  :global-minor-mode t)

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode
  :custom
  (undo-tree-auto-save-history . nil)
)

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless)))

(leaf marginalia
  :ensure t
  :global-minor-mode t)

(leaf consult
  :ensure t
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap goto-line] . consult-goto-line)
   ([remap yank-pop] . consult-yank-pop)
   ("C-;" . consult-buffer)))

(leaf embark
  :ensure t
  :config
  (leaf embark-consult
    :ensure t
    :after consult))

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
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?i ?j ?k ?l))
  :custom-face
  (aw-leading-char-face . '((t (:height 3.0)))))

(leaf which-key
  :global-minor-mode t)

(leaf magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

(leaf recentf
  :init
  (recentf-mode)
  :config
  (setopt recentf-max-saved-items 5000)
  (setopt recentf-auto-cleanup 'never))

(leaf git-gutter
  :ensure t
  :global-minor-mode global-git-gutter-mode
  :custom
  ((git-gutter:added-sign . "++")
   (git-gutter:deleted-sign . "--")
   (git-gutter:modified-sign . "==")))

(leaf treesit
  :config
  (setopt treesit-font-lock-level 4)
  (setopt treesit-language-source-alist
    	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
    	  (css "https://github.com/tree-sitter/tree-sitter-css")
    	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  	  (go "https://github.com/tree-sitter/tree-sitter-go")
  	  (html "https://github.com/tree-sitter/tree-sitter-html")
  	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
  	  (json "https://github.com/tree-sitter/tree-sitter-json")
  	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
  	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
  	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
)

(leaf eglot
  :hook
  (html-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  )

(leaf corfu
  :ensure t
  :global-minor-mode global-corfu-mode
  :custom
  (corfu-cycle . t)
  (corfu-auto . t)
  (text-mode-ispell-word-completion . nil))

(leaf cape
  :ensure t)

(leaf flymake
  :global-minor-mode t)

(leaf project
  :custom
  (project-vc-merge-submodules . nil) ; Git Submoduleは別のプロジェクトとして扱う
  )

(leaf editorconfig
  :global-minor-mode t)

(leaf indent-bars
  :vc (:url "https://github.com/jdtsmith/indent-bars")
  :hook
  prog-mode-hook cc-mode-hook org-mode-hook
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-treesit-support . t)
  (indent-bars-treesit-ignore-blank-lines-types . '("module"))
  (indent-bars-pattern . ".")
  (indent-bars-width-frac . 0.2)
  (indent-bars-pad-frac . 0.2)
  (indent-bars-color-by-depth . '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth . '(:pattern "." :pad 0.1 :width 0.45)))

(add-to-list 'load-path "~/.emacs.d/org-mode-release_9.6.30/lisp")

(leaf org
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   (:org-mode-map
    ("C-c C-;" . org-edit-special))
   (:org-src-mode-map
    ("C-c C-;" . org-edit-src-exit)))
  :mode
  ("\\.org$'" . org-mode)
  :config
  (setopt org-directory "~/src/github.com/grugrut/PersonalProject/")
  :custom
  ;; TODOの状態繊維設定
  (org-todo-keywords . '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "DONE(d)")
			 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "MEETING")))
  (org-todo-keyword-faces . '(("TODO" :foreground "red" :weight bold)
			      ("IN PROGRESS" :foreground "cornflower blue" :weight bold)
			      ("DONE" :foreground "green" :weight bold)))
  (org-log-done . 'time)
  (org-clock-persist . t)
  (org-clock-out-when-done . t)
  (org-adapt-indentation . nil)
  (org-startup-folded . 'fold) 	; 初期表示を折り畳みにする
  )

(leaf org-capture
  :after org
  :commands (org-capture)
  :defvar
  (org-directory)
  :config
  (defvar grugrut/org-inbox-file (concat org-directory "inbox.org"))
  (defvar grugrut/org-journal-file (concat org-directory "journal.org"))
  (setopt org-capture-templates `(
				("t" " Tasks" entry (file ,grugrut/org-inbox-file)
				 "* TODO %? %^G\n:PROPERTIES:\n:DEADLINE: %^{Deadline}T\n:EFFORT: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n")
				("e" " Event" entry (file ,grugrut/org-inbox-file)
				 "* TODO %? %^G\n:PROPERTIES:\n:SCHEDULED: %^{Scheduled}T\n:EFFORT:%^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n")
				("j" " Journal" entry (file+olp+datetree ,grugrut/org-journal-file)
				 "* %<%H:%M> %?")
				("b" " blog" entry
				 (file+headline "~/src/github.com/grugrut/blog/draft/blog.org" ,(format-time-string "%Y"))
				 "** TODO %?\n:PROPERTIES:\n:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :archives '(\"%(format-time-string \"%Y\")\" \"%(format-time-string \"%Y-%m\")\")\n:EXPORT_FILE_NAME: draft\n:END:\n\n")
				)))

(leaf ox-hugo
  :ensure t
  :after ox
  :mode ("\\.org$'" . org-hugo-auto-export-mode))

(leaf typescript-mode
  :ensure t
  :mode
  (("\\.ts\\'" . typescript-mode)
   ("\\.tsx\\'" . tsx-ts-mode)))

(leaf markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . gfm-mode))
  )

(defun grugrut/export-my-init-to-blog ()
  "Export as markdown for my blog post."
  (interactive)
  (require 'ox-hugo)
  (declare-function org-hugo-export-as-md "ox-hugo")
  (let ((file "~/src/github.com/grugrut/blog/content/posts/my-emacs-init-el.md"))
    (org-hugo-export-as-md)
    (write-file file t)))

(leaf win-toast
  :vc (:url "https://github.com/grugrut/win-toast/"))

;;; init.el ends here
