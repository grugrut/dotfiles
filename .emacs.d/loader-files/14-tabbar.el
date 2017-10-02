(use-package tabbar
  :disabled t
  :ensure t
  :bind
  ;; Ctrl+x+カーソル左右で、タブ移動
  (("C-x <right>" . tabbar-forward)
   ("C-x <left>" . tabbar-backward))
  :config
  (tabbar-mode 1)

  ;; グループを使わない
  (setq tabbar-buffer-groups-function nil)

  ;; 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  ;; タブとタブの間隔
  (setq tabbar-separator '(1.0))

  ;; ;; 見た目をきれいに
  (set-face-attribute
   'tabbar-default nil
   :background "#333333"
   :height 0.9)
  (set-face-attribute
   'tabbar-unselected nil
   :foreground "#cccccc"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :foreground "goldenrod"
   :box nil)

  ;; タブに表示するバッファの制御
  (defvar my-tabbar-enable-special-buffer
    '("*scratch*" "*Messages*"))
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda (b)
             (and
              (not (eq b (current-buffer)))
              (not (member (buffer-name b) my-tabbar-enable-special-buffer))
              (find (aref (buffer-name b) 0) " *")
              ))
           (buffer-list)))))
