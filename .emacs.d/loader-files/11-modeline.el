;; 時刻表示
(defvar display-time-string-forms
      '((substring year -2) "/" month "/" day " " 24-hours ":" minutes))
(display-time)


;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos")
(setq eol-mnemonic-mac       ":Mac")
(setq eol-mnemonic-unix      ":Unx")
(setq eol-mnemonic-undecided ":???")

