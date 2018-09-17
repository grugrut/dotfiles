;; 本当はひとつのuse-packageにまとめたいが、spaceline-define-segmentがマクロ展開できないため、先にrequireする
(use-package spaceline-config
  :ensure spaceline)

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
  :tight t :enabled t)

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
