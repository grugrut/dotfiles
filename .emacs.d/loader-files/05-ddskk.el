(use-package ddskk
  :ensure t
  :bind
  (("C-x C-j" . skk-mode)
   ("C-x j" . skk-mode))
  :init
  (defvar dired-bind-jump nil)  ; dired-xがC-xC-jを奪うので対処しておく
  ;; AZIKを使用する
  (defvar skk-use-azik t)
  (defvar skk-azik-keyboard-type 'jp106)

  :config
  (defvar skk-tut-file nil)
  (defvar skk-server-host "localhost")
  (defvar skk-server-portnum 1178)
  ;; 変換時にリターンでは改行しない
  (defvar skk-egg-like-newline t)

  ;; メッセージを日本語にする
  (defvar sskk-japanese-message-and-error t)

  ;; 対応する括弧を自動挿入
  (defvar skk-auto-insert-paren t)

  (defvar skk-check-okurigana-on-touroku t)

  ;; アノテーションを表示
  (defvar skk-show-annotation t)
  (defvar skk-anotation-show-wikipedia-url t)

  ;; 変換候補をインライン表示しない
  (defvar skk-show-tooltip nil)

  ;; isearch時にSKKをオフ
  (defvar skk-isearch-start-mode 'latin)

  ;; 送り仮名を考慮した変換候補
  (defvar skk-henkan-okuri-strictly nil)
  (defvar skk-process-okuri-early nil)
)




