(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :mode ("\\.org$'" . org-mode)
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'system-time-locale) "C")))
  :config
  (setq org-directory "~/org/"))

(use-package org-capture
  :config
  (setq org-capture-templates
        '(
          ("t" "Todo" entry
           (file (concat org-directory "todo.org"))
           "* TODO %?\n %i\n %a\n"
           :prepend nil
           :unnarrowed nil
           :kill-buffer t
           )
          ("m" "Memo" entry
           (file+datetree (concat org-directory "diary.org"))
           "* %?\n %a"
           :prepend t
           :unnarrowed nil
           :kill-buffer t
           )
          ("i" "interrupt" entry
           (file+datetree (concat org-directory "diary.org"))
           "* PHONE %?\n %a"
           :prepend t
           :unnarrowed nil
           :kill-buffer t
           :clock-in t
           :clock-resume t
           )
          ("b" "book memo" entry
           (file (concat org-directory "book.org"))
           "* %^{Book Title} \n%[~/.emacs.d/org-capture-templates/book.txt]")
          )))

;; TODO状態の設定
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("STARTED" :foreground "cornflower blue" :weight bold)
        ("DONE" :foreground "green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "green" :weight bold)
        ("MEETING" :foreground "gren" :weight bold)))

;; 時間計測を開始したらSTARTED状態に
(defvar org-clock-in-switch-to-state 'my-org-clock-in-switch-to-state)

;;; TODOの場合だけSTARTEDに変更する
(defun my-org-clock-in-switch-to-state (state)
  (when (string-equal state "TODO")
    "STARTED"))

(setq org-log-done 'time)
(defvar org-clock-persist t)
(defvar org-clock-out-when-done t)

(use-package org-bullets
  :disabled t
  :ensure
  :init
  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode 1))))

;;; #+UPDATE:を保存時に更新
(use-package time-stamp
  :init
  (add-hook 'before-save-hook 'time-stamp)
  :config
  (setq time-stamp-active t
        time-stamp-line 10
        time-stamp-start "^#\\+LASTMOD:"
        time-stamp-format " %:y-%02m-%02d"
        time-stamp-end "$"))

