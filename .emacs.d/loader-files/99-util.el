(defun my/copy-now-line ()
  "現在ファイルと行をコピーする."
  (interactive)
  (let ((text (format "%s L%d" (buffer-name) (line-number-at-pos))))
    (message text)
    (kill-new text)))

;;; WSLでsuspendが暴発すると復帰できない？ ので確認する
(defun my/confirm-suspend ()
  ""
  (unless (y-or-n-p "Really suspend? ")
    (error "Suspend canceld")))
(add-hook 'suspend-hook 'my/confirm-suspend)
